{-# LANGUAGE GADTs #-}

-- | Live IO interpreter: executes workflows under RWST-style
-- Reader\/Writer\/State threading with an injectable judge backend.
--
-- Reader 'Config' never mutates mid-run; all observations accumulate in the
-- 'Trace' writer and 'TrajState'. Judge errors, empty evidence, and
-- out-of-range calibrations fail closed (boolean contexts read "not passed"),
-- honouring the retry policy first.
module Hydro.Executor
  ( RetryPolicy (..)
  , Config (..)
  , defaultThreshold
  , defaultConfig
  , thresholdFor
  , runWorkflow
  ) where

import Control.Exception (IOException, try)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)

import Hydro.Action (Action (..), ActionOutput (..), summarizeAction, summarizeOutput)
import Hydro.Judge (JudgeBackend (..))
import Hydro.Predicate
  ( Judgment (..)
  , Predicate (..)
  , Question (..)
  , answerPassProb
  , checkPaths
  , evalCheck
  , passJudgment
  )
import Hydro.State (TrajState (..), freshStepId, noteStep)
import Hydro.Trace (EvKind (..), Trace, TraceEvent (..), appendEvent, emptyTrace, resolveEvidence)
import Hydro.Workflow (AskSpec (..), LoopOutcome (..), LoopSpec (..), Workflow (..))

-- | Retry policy: transient judge\/action failures retry up to 'maxRetries'
-- times. Passed checks are never re-run to shop for a different answer.
data RetryPolicy = RetryPolicy
  { maxRetries :: Int
  , retryOnJudgeError :: Bool
  } deriving (Eq, Show)

-- | Static run configuration (the RWST Reader component).
--
-- Thresholds are keyed by judgment statement text (answering the design's
-- DecisionId question with the simplest stable identity); statements absent
-- from 'cfgThresholds' use 'cfgDefaultThreshold'.
data Config = Config
  { cfgJudge :: JudgeBackend
  , cfgThresholds :: Map.Map String Double
  , cfgDefaultThreshold :: Double
  , cfgRetry :: RetryPolicy
  }

-- | Global default pass threshold for judgments without a per-statement entry.
defaultThreshold :: Double
defaultThreshold = 0.7

-- | Config with no per-statement thresholds and zero retries.
defaultConfig :: JudgeBackend -> Config
defaultConfig backend = Config backend Map.empty defaultThreshold (RetryPolicy 0 True)

-- | Effective threshold for a judgment statement.
thresholdFor :: Config -> String -> Double
thresholdFor config statement =
  Map.findWithDefault (cfgDefaultThreshold config) statement (cfgThresholds config)

-- | Execute a workflow, returning its result, final trajectory state, and
-- append-only evidence trace.
runWorkflow :: Config -> TrajState -> Workflow a -> IO (a, TrajState, Trace)
runWorkflow config initial workflow = go initial emptyTrace workflow
  where
    go :: TrajState -> Trace -> Workflow b -> IO (b, TrajState, Trace)
    go st trace step = case step of
      Pure x -> pure (x, st, trace)
      Bind left cont -> do
        (x, st1, trace1) <- go st trace left
        go st1 trace1 (cont x)
      Act action -> do
        out <- runAct (cfgRetry config) action
        let stored = summarizeOutput out ++ "\n" ++ outStdout out ++ outText out
            (sid, st1) = noteStep stored [] st
        pure (out, st1, appendEvent trace (TraceEvent sid EvAction (summarizeAction action) sid))
      Intent note -> do
        let (sid, st1) = noteStep note [] st
        pure ((), st1, appendEvent trace (TraceEvent sid EvIntent note sid))
      Choice predicate onTrue onFalse -> do
        (passed, st1, trace1) <- evalPredicate st trace predicate
        let label = if passed then "then" else "else"
            (sid, st2) = noteStep ("branch: " ++ label) [] st1
            trace2 = appendEvent trace1 (TraceEvent sid EvBranch ("branch: " ++ label) sid)
        go st2 trace2 (if passed then onTrue else onFalse)
      Loop spec predicate body -> runLoop st trace spec predicate body
      Ask spec -> runAsk st trace spec

    runLoop :: TrajState -> Trace -> LoopSpec -> Predicate -> Workflow b -> IO (LoopOutcome b, TrajState, Trace)
    runLoop st trace spec predicate body = do
      let (loopId, st0) = freshStepId st
      iterateLoop st0 trace loopId 0
      where
        iterateLoop stL traceL loopId count
          | count >= maxIters spec = do
              let summary = "loop exhausted after " ++ show count ++ " iters"
                  (sid, stL1) = noteStep summary [] (stL {stLoops = Map.insert loopId count (stLoops stL)})
              pure (Exhausted count, stL1, appendEvent traceL (TraceEvent sid EvLoopIter summary sid))
          | otherwise = do
              (x, stB, traceB) <- go stL traceL body
              (passed, stP, traceP) <- evalPredicate stB traceB predicate
              let next = count + 1
                  summary =
                    "loop iter " ++ show next ++ "/" ++ show (maxIters spec)
                      ++ if passed then " passed" else " continue"
                  (sid, stC) = noteStep summary [] (stP {stLoops = Map.insert loopId next (stLoops stP)})
                  traceC = appendEvent traceP (TraceEvent sid EvLoopIter summary sid)
              if passed
                then pure (Finished x, stC, traceC)
                else iterateLoop stC traceC loopId next

    runAsk :: TrajState -> Trace -> AskSpec b -> IO (b, TrajState, Trace)
    runAsk st trace spec = do
      let (sid, st1) = noteStep ("ask: " ++ askPrompt spec) [] st
          trace1 = appendEvent trace (TraceEvent sid EvAsk (askPrompt spec) sid)
          st2 = st1 {stPendingAsk = Just sid}
      answer <- case askDefault spec of
        Just deflt -> pure deflt
        Nothing -> askLive spec
      pure (answer, st2 {stPendingAsk = Nothing}, trace1)

    evalPredicate :: TrajState -> Trace -> Predicate -> IO (Bool, TrajState, Trace)
    evalPredicate st trace predicate = case predicate of
      PCheck spec -> do
        let paths = checkPaths spec
        flags <- mapM doesFileExist paths
        let fileExists path = maybe False id (lookup path (zip paths flags))
        pure (evalCheck fileExists (stOutputs st) spec, st, trace)
      PJudge judgment -> do
        (passed, prob, threshold) <- evalJudgment st trace judgment
        let summary =
              "judge "
                ++ show prob
                ++ " vs "
                ++ show threshold
                ++ ": "
                ++ judgeStatement judgment
            (sid, st1) = noteStep summary [] st
        pure (passed, st1, appendEvent trace (TraceEvent sid EvJudgment summary sid))

    evalJudgment :: TrajState -> Trace -> Judgment -> IO (Bool, Double, Double)
    evalJudgment st trace judgment = do
      let evidence = resolveEvidence (judgeSelect judgment) trace (stOutputs st) (stTags st)
          threshold = thresholdFor config (judgeStatement judgment)
      if null evidence
        then pure (False, 0, threshold)
        else do
          attempt <- tryJudge config judgment
          let prob = case attempt of
                Right p -> p
                Left _ -> 0
          pure (passJudgment prob threshold, prob, threshold)

-- | Run one judge question with retries, returning the pass probability or
-- the IOException on persistent failure.
tryJudge :: Config -> Judgment -> IO (Either IOException Double)
tryJudge config judgment = go retries
  where
    policy = cfgRetry config
    retries = if retryOnJudgeError policy then maxRetries policy else 0
    go n = do
      attempt <- try (judgeOnce (cfgJudge config) judgment) :: IO (Either IOException Double)
      case attempt of
        Right prob -> pure (Right prob)
        Left err -> if n > 0 then go (n - 1) else pure (Left err)

-- | Dispatch one judgment to the backend by question kind.
judgeOnce :: JudgeBackend -> Judgment -> IO Double
judgeOnce backend judgment = case judgeQuestion judgment of
  QScore -> jbScore backend judgment
  QNoul -> fmap fst (jbNoul backend judgment)
  QChoice options -> fmap answerPassProb (jbChoice backend judgment options)

-- | Run an action, retrying only on IOException (up to 'maxRetries');
-- a non-zero exit is a valid result for checks to inspect, not a failure.
runAct :: RetryPolicy -> Action -> IO ActionOutput
runAct policy action = go (maxRetries policy)
  where
    go n = do
      attempt <- try (runOnce action) :: IO (Either IOException ActionOutput)
      case attempt of
        Right out -> pure out
        Left err ->
          if n > 0
            then go (n - 1)
            else pure (ActionOutput (Just 1) "" ("action failed: " ++ show err))

-- | Single attempt at an action. File reads force their contents so lazy-IO
-- handles close before returning.
runOnce :: Action -> IO ActionOutput
runOnce (Shell cmd args) = do
  (code, out, err) <- readProcessWithExitCode cmd args ""
  let exitCode = case code of
        ExitSuccess -> 0
        ExitFailure n -> n
  pure (ActionOutput (Just exitCode) out err)
runOnce (ReadFile path) = do
  contents <- readFile path
  length contents `seq` pure (ActionOutput (Just 0) "" contents)
runOnce (WriteFile path contents) = do
  writeFile path contents
  pure (ActionOutput (Just 0) "" ("wrote " ++ show (length contents) ++ " chars to " ++ path))
runOnce (EditFile path from to) = do
  contents <- readFile path
  length contents `seq` pure ()
  let (count, edited) = replaceAll from to contents
  writeFile path edited
  pure (ActionOutput (Just 0) "" ("replaced " ++ show count ++ " occurrence(s) in " ++ path))

-- | Replace all non-overlapping occurrences; empty needle is a no-op.
replaceAll :: String -> String -> String -> (Int, String)
replaceAll "" _ haystack = (0, haystack)
replaceAll _ _ [] = (0, [])
replaceAll needle replacement haystack@(first : rest)
  | needle `isPrefixOf` haystack =
      let (count, tailResult) = replaceAll needle replacement (drop (length needle) haystack)
       in (count + 1, replacement ++ tailResult)
  | otherwise =
      let (count, tailResult) = replaceAll needle replacement rest
       in (count, first : tailResult)

-- | Local prefix test (avoids a Data.List import for one use).
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- | Block on stdin\/stdout for an Ask without a default. Re-prompts on
-- undecodable replies; EOF propagates as an IOException.
askLive :: AskSpec a -> IO a
askLive spec = do
  putStrLn (askPrompt spec)
  mapM_ putStrLn [show (n :: Int) ++ ". " ++ option | (n, option) <- zip [1 ..] (askOptions spec)]
  hFlush stdout
  loopPrompt
  where
    loopPrompt = do
      line <- getLine
      case askDecode spec line of
        Just answer -> pure answer
        Nothing -> do
          putStrLn "Unrecognized answer; please try again."
          loopPrompt
