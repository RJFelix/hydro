-- | Branch-fork interpreter: run choice branches against forked copies of
-- the pure trajectory state, then commit one winner.
--
-- Merge semantics: the snapshot is a pure value, so forking copies
-- nothing; each fork runs via 'runWorkflow' from the snapshot with an
-- empty trace, producing a suffix. Committing keeps
-- @prefix ++ winner-suffix ++ [commit-event]@ and discards the loser's
-- suffix and state. Both forks allocate step ids from the same snapshot
-- counter, so their suffixes overlap; only committed ids survive, which
-- preserves id uniqueness in the final trace. The winner's 'TrajState' is
-- adopted wholesale (no field-wise merge).
--
-- Safety note: only effect-free or read-only branches ('Intent', file
-- reads, side-effect-free shell commands, pure checks) are fork-safe under
-- 'exploreBranches', because both branches' IO effects execute for real
-- with no sandbox or rollback. 'WriteFile', 'EditFile', side-effecting
-- shell commands, and 'Ask' without a default (which blocks on stdin in
-- each fork) are NOT fork-safe to explore. 'forkChoice' evaluates the
-- predicate first and runs only the winner, so it is exactly as safe as
-- the live executor.
--
-- Gap (documented, no parallel variant): there is deliberately no
-- 'Control.Concurrent' parallel-branches variant. Parallel forks would
-- interleave filesystem and stdin effects and race on the id counter with
-- no sandbox to contain them; forks are explored sequentially, then-branch
-- first, per the design's v1 non-goals.
module Hydro.Fork
  ( ForkDecision (..)
  , ForkOutcome (..)
  , evalForkPredicate
  , exploreBranches
  , commitFork
  , forkChoice
  ) where

import Control.Exception (IOException, try)
import System.Directory (doesFileExist)

import Hydro.Executor (Config (..), RetryPolicy (..), runWorkflow, thresholdFor)
import Hydro.Judge (JudgeBackend (..))
import Hydro.Predicate (Judgment (..), Predicate (..), Question (..), answerPassProb, checkPaths, evalCheck, passJudgment)
import Hydro.State (TrajState (..), noteStep)
import Hydro.Trace (EvKind (..), Trace, TraceEvent (..), resolveEvidence)
import Hydro.Workflow (Workflow)

-- | Which branch won a fork: the then-branch or the else-branch.
data ForkDecision
  = ForkThen
  | ForkElse
  deriving (Eq, Show)

-- | One explored fork: its value, its final state, and its trace suffix
-- (which starts empty, so suffixes from two forks of one snapshot share
-- the id space until one is committed).
data ForkOutcome a = ForkOutcome
  { outcomeValue :: a
  , outcomeState :: TrajState
  , outcomeTrace :: Trace
  }

-- | Evaluate a predicate against pre-fork state and trace, mirroring the
-- live executor's fail-closed rules: empty evidence, judge errors, and
-- persistent failures all read as 'False' after honouring the retry
-- policy. Branch-produced evidence is never visible here; only the
-- prefix is.
evalForkPredicate :: Config -> TrajState -> Trace -> Predicate -> IO Bool
evalForkPredicate config st trace predicate = case predicate of
  PCheck spec -> do
    let paths = checkPaths spec
    flags <- mapM doesFileExist paths
    let fileExists path = maybe False id (lookup path (zip paths flags))
    pure (evalCheck fileExists (stOutputs st) spec)
  PJudge judgment -> do
    let evidence = resolveEvidence (judgeSelect judgment) trace (stOutputs st) (stTags st)
        threshold = thresholdFor config (judgeStatement judgment)
    if null evidence
      then pure False
      else do
        attempt <- retryJudge (cfgRetry config) (judgeOnce (cfgJudge config) judgment)
        pure $ case attempt of
          Right prob -> passJudgment prob threshold
          Left _ -> False

-- | Explore both branches sequentially (then-branch first) against forked
-- copies of the snapshot. Both branches' effects execute: only use with
-- read-only or idempotent branches (see the module safety note).
exploreBranches :: Config -> TrajState -> Workflow a -> Workflow a -> IO (ForkOutcome a, ForkOutcome a)
exploreBranches config snapshot onTrue onFalse = do
  (thenVal, thenState, thenTrace) <- runWorkflow config snapshot onTrue
  (elseVal, elseState, elseTrace) <- runWorkflow config snapshot onFalse
  pure (ForkOutcome thenVal thenState thenTrace, ForkOutcome elseVal elseState elseTrace)

-- | Commit one explored fork: adopt the winner's value and state, keep
-- @prefix ++ winner-suffix@, append a commit event recording the decision
-- and the discarded loser, and drop the loser's suffix and state.
commitFork :: Trace -> ForkDecision -> (ForkOutcome a, ForkOutcome a) -> (a, TrajState, Trace)
commitFork prefix decision (thenOut, elseOut) =
  let winner = case decision of
        ForkThen -> thenOut
        ForkElse -> elseOut
      label = case decision of
        ForkThen -> "then"
        ForkElse -> "else"
      summary = "fork: committed " ++ label ++ "-branch; loser suffix discarded"
      (sid, st1) = noteStep summary [] (outcomeState winner)
      event = TraceEvent sid EvBranch summary sid
   in (outcomeValue winner, st1, prefix ++ outcomeTrace winner ++ [event])

-- | Forking choice: evaluate the predicate against the pre-fork
-- state\/trace, then run only the winning branch from a forked copy and
-- commit it with a branch event. Runs winner effects exactly once, like
-- the live executor.
forkChoice :: Config -> TrajState -> Trace -> Predicate -> Workflow a -> Workflow a -> IO (a, TrajState, Trace)
forkChoice config snapshot prefix predicate onTrue onFalse = do
  passed <- evalForkPredicate config snapshot prefix predicate
  let (label, branch) =
        if passed
          then ("then", onTrue)
          else ("else", onFalse)
      summary = "fork branch: " ++ label
      (sid, st1) = noteStep summary [] snapshot
      event = TraceEvent sid EvBranch summary sid
  (value, st2, suffix) <- runWorkflow config st1 branch
  pure (value, st2, prefix ++ [event] ++ suffix)

-- | Run one judge question with retries, returning the pass probability or
-- the IOException on persistent failure. Mirrors the live executor; kept
-- separate so this step does not modify the executor's code.
retryJudge :: RetryPolicy -> IO Double -> IO (Either IOException Double)
retryJudge policy action = go (maxRetries policy)
  where
    go n = do
      attempt <- try action
      case attempt of
        Right prob -> pure (Right prob)
        Left err -> if n > 0 then go (n - 1) else pure (Left err)

-- | Dispatch one judgment to the backend by question kind.
judgeOnce :: JudgeBackend -> Judgment -> IO Double
judgeOnce backend judgment = case judgeQuestion judgment of
  QScore -> jbScore backend judgment
  QNoul -> fmap fst (jbNoul backend judgment)
  QChoice options -> fmap answerPassProb (jbChoice backend judgment options)
