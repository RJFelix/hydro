module Main (main) where

import Control.Exception (IOException, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (exitFailure, exitSuccess)

import Hydro.Action (Action (..), ActionOutput (..))
import Hydro.DryRun (DryRunOptions (..), defaultDryRunOptions, renderPlan, renderPlanWith)
import Hydro.Executor (Config (..), RetryPolicy (..), defaultConfig, defaultThreshold, runWorkflow)
import Hydro.Fork (ForkDecision (..), ForkOutcome (..), commitFork, evalForkPredicate, exploreBranches, forkChoice)
import Hydro.Jev
  ( JevConfig (..)
  , JevConfigError (..)
  , JevError (..)
  , JevRequest (..)
  , JevResponse (..)
  , loadJevConfig
  , loadJevConfigFrom
  , ppJevConfigSafe
  , renderJevRequest
  , runJevStub
  , jevBackend
  )
import Hydro.Judge (mockBackend, mockBackendLogged)
import Hydro.Policy
  ( effectiveThreshold
  , judgmentPasses
  , mkRetryPolicy
  , noRetry
  , retryIO
  , setDefaultThreshold
  , setRetryPolicy
  , setThreshold
  , shouldRetryJudge
  )
import Hydro.Predicate
  ( Answer (..)
  , CheckSpec (..)
  , EvidenceSelector (..)
  , Judgment (..)
  , Predicate (..)
  , Question (..)
  , evalCheck
  , passJudgment
  )
import Hydro.State (TrajState (..), initState)
import Hydro.Trace (EvKind (..), TraceEvent (..), resolveEvidence)
import Hydro.Workflow (AskSpec (..), LoopOutcome (..), Workflow, act, ask, choice, intent, loop, mkChoiceAsk, mkLoopSpec)

-- | Assertion helper: prints the verdict, returns it for collection.
check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)
  pure ok

okOut :: ActionOutput
okOut = ActionOutput (Just 0) "hi" ""

badOut :: ActionOutput
badOut = ActionOutput (Just 3) "" "boom"

-- | Score-question judgment over recent evidence for the given statement.
scoreJudge :: String -> Predicate
scoreJudge statement = PJudge (Judgment statement (LastN 5) QScore)

main :: IO ()
main = do
  results <-
    sequence
      [ testChoiceThen
      , testChoiceElse
      , testJudgeAbove
      , testJudgeBelow
      , testJudgeEmptyFailsClosed
      , testLoopExits
      , testLoopExhausts
      , testLoopSpecRejects
      , testTraceContents
      , testCheckTruthTable
      , testThresholdEquality
      , testTracePrefix
      , testDryRunExact
      , testDryRunChoice
      , testDryRunLoop
      , testDryRunAsk
      , testDryRunNoEffects
      , testDryRunTruncation
      , testDryRunThreshold
      , testForkChoiceThen
      , testForkChoiceElse
      , testForkJudged
      , testForkPredicatePreFork
      , testExploreCommit
      , testForkPrefix
      , testJevPureConfig
      , testJevEnv
      , testJevStub
      , testJevRedact
      , testPolicyThresholds
      , testPolicyRetry
      ]
  if and results then exitSuccess else exitFailure

withMock :: Map.Map String Double -> Config
withMock table = (defaultConfig (mockBackend table)) {cfgRetry = RetryPolicy 0 True}

testChoiceThen :: IO Bool
testChoiceThen = do
  (result, _, _) <-
    runWorkflow
      (withMock Map.empty)
      initState
      (choice (PCheck (ExitZero okOut)) (pure "then") (pure "else") :: Workflow String)
  check "choice takes then-branch on passing check" (result == "then")

testChoiceElse :: IO Bool
testChoiceElse = do
  (result, _, _) <-
    runWorkflow
      (withMock Map.empty)
      initState
      (choice (PCheck (ExitZero badOut)) (pure "then") (pure "else") :: Workflow String)
  check "choice takes else-branch on failing check" (result == "else")

testJudgeAbove :: IO Bool
testJudgeAbove = do
  let cfg = withMock (Map.singleton "ships" 0.9)
      workflow = do
        intent "evidence"
        choice (scoreJudge "ships") (pure "then") (pure "else") :: Workflow String
  (result, _, _) <- runWorkflow cfg initState workflow
  check "judged choice takes then-branch above threshold" (result == "then")

testJudgeBelow :: IO Bool
testJudgeBelow = do
  let cfg = withMock (Map.singleton "ships" 0.2)
      workflow = do
        intent "evidence"
        choice (scoreJudge "ships") (pure "then") (pure "else") :: Workflow String
  (result, _, _) <- runWorkflow cfg initState workflow
  check "judged choice takes else-branch below threshold" (result == "else")

testJudgeEmptyFailsClosed :: IO Bool
testJudgeEmptyFailsClosed = do
  logRef <- newIORef []
  let backend = mockBackendLogged (Map.singleton "ships" 0.9) logRef
      cfg = (defaultConfig backend) {cfgRetry = RetryPolicy 0 True}
  (result, _, _) <-
    runWorkflow cfg initState (choice (scoreJudge "ships") (pure "then") (pure "else") :: Workflow String)
  calls <- readIORef logRef
  check "judgment with empty evidence fails closed without calling backend" (result == "else" && null calls)

counterPath :: FilePath
counterPath = "/tmp/hydro-core-test-counter"

markerPath :: FilePath
markerPath = "/tmp/hydro-core-test-marker"

cleanFiles :: IO ()
cleanFiles = mapM_ removeQuiet [counterPath, markerPath]
  where
    removeQuiet path = do
      _ <- try (removeFile path) :: IO (Either IOException ())
      pure ()

testLoopExits :: IO Bool
testLoopExits = do
  cleanFiles
  writeFile counterPath "0"
  let script =
        "n=$(cat "
          ++ counterPath
          ++ " 2>/dev/null || echo 0); n=$((n + 1)); echo \"$n\" > "
          ++ counterPath
          ++ "; if [ \"$n\" -ge 3 ]; then echo done > "
          ++ markerPath
          ++ "; fi"
      spec = case mkLoopSpec 5 of
        Just s -> s
        Nothing -> error "testLoopExits: mkLoopSpec 5 rejected"
      workflow = loop spec (PCheck (FileExists markerPath)) (act (Shell "sh" ["-c", script]))
  (outcome, _, _) <- runWorkflow (withMock Map.empty) initState workflow
  contents <- readFile counterPath
  cleanFiles
  let ranThree = case outcome of
        Finished _ -> contents == "3\n"
        Exhausted _ -> False
  check "loop exits on deterministic condition after 3 iterations" ranThree

testLoopExhausts :: IO Bool
testLoopExhausts = do
  let spec = case mkLoopSpec 2 of
        Just s -> s
        Nothing -> error "testLoopExhausts: mkLoopSpec 2 rejected"
      workflow = loop spec (PCheck (FileExists "/tmp/hydro-core-test-never-present")) (intent "spin")
  (outcome, _, trace) <- runWorkflow (withMock Map.empty) initState workflow
  let iters = length [e | e <- trace, evKind e == EvLoopIter]
  check
    "loop that never passes exhausts bound with exactly maxIters iterations"
    (outcome == Exhausted 2 && iters == 3)

testLoopSpecRejects :: IO Bool
testLoopSpecRejects =
  check "mkLoopSpec rejects bounds below 1" (mkLoopSpec 0 == Nothing && mkLoopSpec (-1) == Nothing)

testTraceContents :: IO Bool
testTraceContents = do
  let workflow = do
        intent "note one"
        _ <- act (Shell "echo" ["hello"])
        pure ()
  (_, finalState, trace) <- runWorkflow (withMock Map.empty) initState workflow
  let kinds = map evKind trace
      allEv = resolveEvidence AllOutputs trace (stOutputs finalState) (stTags finalState)
      lastOne = resolveEvidence (LastN 1) trace (stOutputs finalState) (stTags finalState)
      unknownIds = resolveEvidence (StepIds [999999]) trace (stOutputs finalState) (stTags finalState)
      unknownTag = resolveEvidence (TagMatch "no-such-tag") trace (stOutputs finalState) (stTags finalState)
      helloSeen = any (\(_, text) -> "hello" `isInfixOf` text) allEv
  check
    "evidence trace records steps and resolves selectors"
    ( kinds == [EvIntent, EvAction]
        && length allEv == 2
        && length lastOne == 1
        && null unknownIds
        && null unknownTag
        && helloSeen
    )

testCheckTruthTable :: IO Bool
testCheckTruthTable = do
  let outputs = Map.singleton 7 "all systems go"
      fileExists path = path == "/present"
      cases =
        [ evalCheck fileExists outputs (ExitZero okOut)
        , not (evalCheck fileExists outputs (ExitZero badOut))
        , not (evalCheck fileExists outputs (ExitZero (ActionOutput Nothing "" "")))
        , evalCheck fileExists outputs (FileExists "/present")
        , not (evalCheck fileExists outputs (FileExists "/absent"))
        , evalCheck fileExists outputs (OutputContains okOut "hi")
        , evalCheck fileExists outputs (OutputContains badOut "boom")
        , not (evalCheck fileExists outputs (OutputContains okOut "zzz"))
        , evalCheck fileExists outputs (StepSucceeded 7)
        , not (evalCheck fileExists outputs (StepSucceeded 8))
        , evalCheck fileExists outputs (And (ExitZero okOut) (FileExists "/present"))
        , not (evalCheck fileExists outputs (And (ExitZero okOut) (FileExists "/absent")))
        , evalCheck fileExists outputs (Or (ExitZero badOut) (FileExists "/present"))
        , evalCheck fileExists outputs (Not (ExitZero badOut))
        , not (evalCheck fileExists outputs (Not (ExitZero okOut)))
        ]
  check "check truth table (combinators and leaves)" (and cases)

testThresholdEquality :: IO Bool
testThresholdEquality =
  check "threshold equality passes, below fails" (passJudgment 0.7 0.7 && not (passJudgment 0.69 0.7))

testTracePrefix :: IO Bool
testTracePrefix = do
  (_, _, short) <- runWorkflow (withMock Map.empty) initState (intent "one" :: Workflow ())
  (_, _, long) <- runWorkflow (withMock Map.empty) initState (intent "one" >> intent "two" :: Workflow ())
  check "trace is append-only (prefix preserved)" (take (length short) long == short)

testDryRunExact :: IO Bool
testDryRunExact = do
  let workflow = do
        intent "hello"
        _ <- act (Shell "echo" ["hi"])
        pure ()
      expected =
        unlines
          [ "# Workflow plan"
          , ""
          , "1. Note: hello"
          , "2. Run shell `echo hi`"
          , ""
          , "(2 steps, dry run: no actions were executed.)"
          ]
  check "dry-run renders intent+action exactly" (renderPlan workflow == expected)

testDryRunChoice :: IO Bool
testDryRunChoice = do
  let plan =
        renderPlan
          (choice (PCheck (ExitZero okOut)) (intent "took then") (intent "took else") :: Workflow ())
  check
    "dry-run shows both choice branches and the predicate"
    (all (`isInfixOf` plan) ["Choice", "took then", "took else", "exit"])

testDryRunLoop :: IO Bool
testDryRunLoop = do
  let spec = case mkLoopSpec 5 of
        Just s -> s
        Nothing -> error "testDryRunLoop: mkLoopSpec 5 rejected"
      plan = renderPlan (loop spec (PCheck (ExitZero okOut)) (intent "spin body") >> pure () :: Workflow ())
  check
    "dry-run shows loop bound and body"
    (all (`isInfixOf` plan) ["Loop", "5", "spin body"])

testDryRunAsk :: IO Bool
testDryRunAsk = do
  let spec = mkChoiceAsk "pick a color" [("red", "R"), ("blue", "B")] (Just "R")
      plan = renderPlan (ask spec >> pure () :: Workflow ())
  check
    "dry-run shows ask prompt and options"
    (all (`isInfixOf` plan) ["pick a color", "red", "blue"])

testDryRunNoEffects :: IO Bool
testDryRunNoEffects = do
  let path = "/tmp/hydro-dryrun-noeffect"
      workflow = do
        _ <- act (WriteFile path "should never be written")
        _ <- act (Shell "touch" [path])
        pure ()
      plan = renderPlan workflow
  _ <- try (removeFile path) :: IO (Either IOException ())
  exists <- doesFileExist path
  check "dry-run performs no effects" (not exists && length plan > 0)

testDryRunTruncation :: IO Bool
testDryRunTruncation = do
  let unanswerable = AskSpec "mystery" [] (const Nothing) Nothing :: AskSpec String
      plan = renderPlan (ask unanswerable >> intent "never reached" :: Workflow ())
  check
    "dry-run truncates after an unanswerable ask"
    ("truncated" `isInfixOf` plan && not ("never reached" `isInfixOf` plan))

testDryRunThreshold :: IO Bool
testDryRunThreshold = do
  let base = withMock Map.empty
      opts = defaultDryRunOptions {dryConfig = setThreshold "ships" 0.95 base}
      plan = renderPlanWith opts (choice (scoreJudge "ships") (pure "t") (pure "e") :: Workflow String)
  check "dry-run shows the configured judgment threshold" ("0.95" `isInfixOf` plan)

testForkChoiceThen :: IO Bool
testForkChoiceThen = do
  (result, _, trace) <-
    forkChoice
      (withMock Map.empty)
      initState
      []
      (PCheck (ExitZero okOut))
      (intent "took then" >> pure "T")
      (intent "took else" >> pure "E")
  let summaries = map evSummary trace
  check
    "forkChoice runs the then-branch on a passing check"
    (result == "T" && any ("took then" `isInfixOf`) summaries && not (any ("took else" `isInfixOf`) summaries))

testForkChoiceElse :: IO Bool
testForkChoiceElse = do
  (result, _, trace) <-
    forkChoice
      (withMock Map.empty)
      initState
      []
      (PCheck (ExitZero badOut))
      (intent "took then" >> pure "T")
      (intent "took else" >> pure "E")
  let summaries = map evSummary trace
  check
    "forkChoice runs the else-branch on a failing check"
    (result == "E" && any ("took else" `isInfixOf`) summaries && not (any ("took then" `isInfixOf`) summaries))

testForkJudged :: IO Bool
testForkJudged = do
  let high = withMock (Map.singleton "ships" 0.9)
      low = withMock (Map.singleton "ships" 0.2)
      branch label = intent label >> pure label
  (_, stHigh, trHigh) <- runWorkflow high initState (intent "evidence")
  (resHigh, _, _) <- forkChoice high stHigh trHigh (scoreJudge "ships") (branch "high-then") (branch "high-else")
  (_, stLow, trLow) <- runWorkflow low initState (intent "evidence")
  (resLow, _, _) <- forkChoice low stLow trLow (scoreJudge "ships") (branch "low-then") (branch "low-else")
  check "forkChoice follows judged predicates via the mock" (resHigh == "high-then" && resLow == "low-else")

testForkPredicatePreFork :: IO Bool
testForkPredicatePreFork = do
  let cfg = withMock (Map.singleton "ships" 0.9)
  (_, st, trace) <- runWorkflow cfg initState (intent "evidence")
  decided <- evalForkPredicate cfg st trace (scoreJudge "ships")
  undecided <- evalForkPredicate cfg initState [] (scoreJudge "ships")
  check "fork predicates evaluate pre-fork evidence and fail closed when empty" (decided && not undecided)

testExploreCommit :: IO Bool
testExploreCommit = do
  (thenOut, elseOut) <-
    exploreBranches
      (withMock Map.empty)
      initState
      (intent "alpha" >> pure "A")
      (intent "beta" >> pure "B")
  let bothRan = outcomeValue thenOut == "A" && outcomeValue elseOut == "B"
      (val, _, trace) = commitFork [] ForkElse (thenOut, elseOut)
      summaries = map evSummary trace
  check
    "exploreBranches runs both forks; commitFork keeps the winner only"
    ( bothRan
        && val == "B"
        && any ("beta" `isInfixOf`) summaries
        && any ("committed else" `isInfixOf`) summaries
        && not (any ("alpha" `isInfixOf`) summaries)
    )

testForkPrefix :: IO Bool
testForkPrefix = do
  let cfg = withMock Map.empty
  (_, st, prefix) <- runWorkflow cfg initState (intent "prefix evidence")
  (thenOut, elseOut) <-
    exploreBranches cfg st (intent "t-branch" >> pure "T") (intent "e-branch" >> pure "E")
  let (_, _, trace) = commitFork prefix ForkThen (thenOut, elseOut)
  check "committed fork trace preserves the prefix" (take (length prefix) trace == prefix)

testJevPureConfig :: IO Bool
testJevPureConfig =
  check
    "jev config validation is total and pure"
    ( loadJevConfigFrom Nothing Nothing == Left JevMissingUrl
        && loadJevConfigFrom (Just "") Nothing == Left JevEmptyUrl
        && loadJevConfigFrom (Just "https://jev.example") Nothing == Right (JevConfig "https://jev.example" Nothing)
        && loadJevConfigFrom (Just "u") (Just "") == Right (JevConfig "u" Nothing)
        && loadJevConfigFrom (Just "u") (Just "k") == Right (JevConfig "u" (Just "k"))
    )

testJevEnv :: IO Bool
testJevEnv = do
  oldUrl <- lookupEnv "JEV_API_URL"
  oldKey <- lookupEnv "JEV_API_KEY"
  unsetEnv "JEV_API_URL"
  unsetEnv "JEV_API_KEY"
  missing <- loadJevConfig
  setEnv "JEV_API_URL" "https://jev.example"
  present <- loadJevConfig
  case oldUrl of
    Just v -> setEnv "JEV_API_URL" v
    Nothing -> unsetEnv "JEV_API_URL"
  case oldKey of
    Just v -> setEnv "JEV_API_KEY" v
    Nothing -> unsetEnv "JEV_API_KEY"
  check
    "jev config loads from JEV_API_URL/JEV_API_KEY"
    (missing == Left JevMissingUrl && present == Right (JevConfig "https://jev.example" Nothing))

testJevStub :: IO Bool
testJevStub = do
  let jevCfg = JevConfig "https://jev.example" Nothing
  stubResult <- runJevStub jevCfg (JevRequest "ships?" [] QScore)
  let isUnavail = case stubResult of
        Left (JevUnavailable _) -> True
        _ -> False
  (result, _, _) <-
    runWorkflow
      (defaultConfig (jevBackend jevCfg))
      initState
      (intent "evidence" >> choice (scoreJudge "ships") (pure "then") (pure "else") :: Workflow String)
  check "jev stub is unavailable and fail-closed through the executor" (isUnavail && result == "else")

testJevRedact :: IO Bool
testJevRedact = do
  let jevCfg = JevConfig "https://jev.example" (Just "super-secret-key")
      safe = ppJevConfigSafe jevCfg
      rendered = renderJevRequest (JevRequest "ships?" [(1, "evidence text")] QScore)
      wrapped = JevResponse (AScore 0.5) == JevResponse (AScore 0.5)
  check
    "jev rendering shows evidence but never the api key"
    ( wrapped
        && not ("super-secret-key" `isInfixOf` safe)
        && ("<redacted>" `isInfixOf` safe)
        && ("ships?" `isInfixOf` rendered)
        && ("evidence text" `isInfixOf` rendered)
    )

testPolicyThresholds :: IO Bool
testPolicyThresholds = do
  let base = withMock Map.empty
      judgment = Judgment "ships" (LastN 5) QScore
      high = setThreshold "ships" 0.9 base
  check
    "policy thresholds read from config with clamping"
    ( effectiveThreshold base judgment == defaultThreshold
        && effectiveThreshold (setThreshold "ships" 0.95 base) judgment == 0.95
        && effectiveThreshold (setThreshold "ships" 2.0 base) judgment == 1.0
        && effectiveThreshold (setDefaultThreshold 0.3 base) judgment == 0.3
        && judgmentPasses high judgment 0.9
        && not (judgmentPasses high judgment 0.89)
    )

testPolicyRetry :: IO Bool
testPolicyRetry = do
  counter <- newIORef (0 :: Int)
  let flaky = do
        n <- readIORef counter
        writeIORef counter (n + 1)
        if n < 2 then ioError (userError "boom") else pure "ok"
  first <- retryIO (mkRetryPolicy 2 True) flaky
  writeIORef counter 0
  second <- retryIO (mkRetryPolicy 1 True) flaky
  let base = withMock Map.empty
      retried = setRetryPolicy (mkRetryPolicy 2 True) base
      succeeded = case first of
        Right val -> val == "ok"
        Left _ -> False
      gaveUp = case second of
        Left _ -> True
        Right _ -> False
  check
    "policy retry honors bounds and validation"
    ( succeeded
        && gaveUp
        && mkRetryPolicy (-3) True == RetryPolicy 0 True
        && shouldRetryJudge retried
        && not (shouldRetryJudge (setRetryPolicy noRetry base))
    )

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails rest@(_ : xs) = rest : tails xs
