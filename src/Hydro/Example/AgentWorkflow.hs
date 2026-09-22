-- | Roleplay validation: a coding agent living inside the harness whose
-- only tool is the Hydro DSL authors a project-health workflow.
--
-- The job: build the project, run its tests, retry once on failure via a
-- bounded loop, branch on the outcome (deterministic marker check first,
-- free-form judge fallback second), offer the operator a publish decision,
-- and leave intent notes a future agent can pick up.
module Hydro.Example.AgentWorkflow
  ( healthMarker
  , retryTwice
  , flakyFallback
  , healthAttempt
  , publishAsk
  , projectHealth
  ) where

import Hydro.Action (Action (..))
import Hydro.Predicate (CheckSpec (..), EvidenceSelector (..), Judgment (..), Predicate (..), Question (..))
import Hydro.Workflow (AskSpec, LoopSpec (..), Workflow, act, ask, choice, intent, loop, mkChoiceAsk, mkLoopSpec)

-- | Marker file one successful attempt writes. The loop condition and the
-- post-loop branch both read it via 'FileExists': a 'CheckSpec' captures
-- values fixed at authoring time, so the body's fresh exit codes cannot be
-- named by the loop predicate itself (see the step report).
healthMarker :: FilePath
healthMarker = "/tmp/hydro-health-ok"

-- | One initial attempt plus one retry. 'mkLoopSpec' rejects bounds below
-- @1@, so the total case below keeps authoring total.
retryTwice :: LoopSpec
retryTwice = case mkLoopSpec 2 of
  Just spec -> spec
  Nothing -> LoopSpec 2

-- | Free-form fallback: when the deterministic marker check stays red, ask
-- the judge whether the accumulated log reads as transient flakes.
-- 'LastN' keeps the priced evidence small; 'StepIds' cannot be used here
-- because step ids are allocated at runtime.
flakyFallback :: Predicate
flakyFallback =
  PJudge
    ( Judgment
        "The captured build and test log shows only transient or environment flakes, so the failure is safe to treat as non-blocking."
        (LastN 10)
        QScore
    )

-- | One health attempt: clear a stale marker, build, then test. Each stage
-- branches on the exit code just bound (the 'ExitZero' pattern from the
-- demo workflow); only a green test run rewrites the marker.
healthAttempt :: Workflow String
healthAttempt = do
  _ <- act (Shell "rm" ["-f", healthMarker])
  intent "health attempt: building the project (cabal build all)"
  buildOut <- act (Shell "cabal" ["build", "all"])
  choice
    (PCheck (ExitZero buildOut))
    ( do
        intent "build green; running the test suite (cabal test all)"
        testOut <- act (Shell "cabal" ["test", "all"])
        choice
          (PCheck (ExitZero testOut))
          ( do
              _ <- act (Shell "touch" [healthMarker])
              intent "tests green; health marker written for the loop check"
              pure "attempt: build and tests passed"
          )
          ( do
              intent "tests red; marker left absent so the bounded loop retries"
              pure "attempt: tests failed"
          )
    )
    ( do
        intent "build red; marker left absent so the bounded loop retries"
        pure "attempt: build failed"
    )

-- | Operator decision with a default, so headless runs never block on stdin.
publishAsk :: AskSpec Bool
publishAsk = mkChoiceAsk "Publish the health report?" [("yes", True), ("no", False)] (Just False)

-- | Full job: bounded retries, deterministic branch on the marker, judged
-- fallback when still red, and a publish question when green.
projectHealth :: Workflow String
projectHealth = do
  intent "agent plan: verify project health via build plus tests, retry once on failure, then report for a future agent"
  _ <- loop retryTwice (PCheck (FileExists healthMarker)) healthAttempt
  intent "attempts finished; branching on the health marker"
  choice
    (PCheck (FileExists healthMarker))
    ( do
        intent "project healthy; asking the operator whether to publish the report"
        publish <- ask publishAsk
        if publish
          then do
            intent "operator confirmed publish; a future agent can wire the upload transport"
            pure "healthy: report published"
          else do
            intent "operator deferred publish; report kept local for a future agent"
            pure "healthy: report kept local"
    )
    ( do
        intent "marker absent after retries; consulting the judge on whether the log looks flaky"
        choice
          flakyFallback
          ( do
              intent "judge reads the failure as transient; reporting degraded-green for a future agent to re-check"
              pure "degraded: judge reads the failure as transient"
          )
          ( do
              intent "judge reads the failure as real; recording a repair request for a future agent"
              pure "unhealthy: needs repair"
          )
    )
