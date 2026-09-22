# Hydro — Design (v1, design only, no implementation)

Vision: a coding-agent harness whose agent's only tool is a Haskell DSL for
ad-hoc workflows. The agent composes ordinary actions, notes to itself or
other agents, deterministic or judged branches, bounded loops, and user
questions into a `Workflow` value; interpreters execute it, dry-run it as a
markdown plan, or fork it for speculative execution.

Constraints (repo): Cabal package `hydro`, GHC 9.10.3, GHC2021, `-Wall` with
zero warnings. Dependencies restricted to `base` plus already-installed GHC
boot libraries (observed via `ghc-pkg list`: `containers`, `text`,
`bytestring`, `directory`, `filepath`, `process`, `mtl`, `transformers`,
`stm`, `time`, `pretty`, `exceptions`, `unix`, `parsec`, among others). No
Hackage additions. `MyLib` / `app/Main` / `test/Main` are untouched by this
design step.

## (a) Core workflow ADT

One constructor per construct in the vision. `Workflow` is a monad (via
`Pure` / `Bind`) so ad-hoc workflows sequence with do-notation; domain steps
are the remaining constructors.

```haskell
data Action
  = Shell { shellCmd :: String, shellArgs :: [String] }
  | ReadFile { readPath :: FilePath }
  | WriteFile { writePath :: FilePath, writeContents :: String }
  | EditFile { editPath :: FilePath, editFrom :: String, editTo :: String }

data Predicate
  = PCheck CheckSpec
  | PJudge Judgment

data AskSpec a  -- prompt plus offered decisions; result decodes the user's pick
data LoopSpec   -- bound only; condition lives in the Predicate argument

data Workflow a where
  Pure   :: a -> Workflow a
  Bind   :: Workflow a -> (a -> Workflow b) -> Workflow b
  Act    :: Action -> Workflow ActionOutput
  Intent :: String -> Workflow ()              -- note to future-self / agents
  Choice :: Predicate -> Workflow a -> Workflow a -> Workflow a
  Loop   :: LoopSpec -> Predicate -> Workflow a -> Workflow (LoopOutcome a)
  Ask    :: AskSpec a -> Workflow a
```

Constructor-to-construct mapping: `Act` = ordinary actions (shell, reads,
edits); `Intent` = intents/notes; `Choice` = choices (deterministic check vs
free-form judgment distinguished by the `Predicate` argument, one continued
workflow per branch); `Loop` = loops (repeat body until the `Predicate`
passes, capped by `LoopSpec`); `Ask` = soliciting user input / offering user
decisions. `Pure`/`Bind` are plumbing, not domain constructs.

Key supporting types:

```haskell
data ActionOutput = ActionOutput
  { outExit :: Maybe Int, outStdout :: String, outText :: String }
data LoopOutcome a = Finished a | Exhausted Int  -- result or bound hit + tries
data LoopSpec = LoopSpec { maxIters :: Int }     -- required, >= 1; 0 rejected
```

`Loop` semantics: run body, evaluate predicate, stop on pass (`Finished`) or
when `maxIters` is reached (`Exhausted`). The bound is mandatory so every
loop terminates even if a judgment never passes.

## (b) Check-vs-Judgment predicate split

```haskell
data CheckSpec
  = ExitZero ActionOutput   -- deterministic re-check of a just-run action
  | FileExists FilePath
  | OutputContains ActionOutput String
  | StepSucceeded StepId
  | And CheckSpec CheckSpec | Or CheckSpec CheckSpec | Not CheckSpec

data Judgment = Judgment
  { judgeStatement :: String          -- free-form claim to evaluate
  , judgeSelect    :: EvidenceSelector -- which upstream outputs to send
  , judgeQuestion  :: Question        -- Choice / Score / Noul (see below)
  }

data EvidenceSelector
  = AllOutputs
  | LastN Int
  | StepIds [StepId]
  | TagMatch String
```

`CheckSpec` is pure, total, local, and free: it inspects values already in
hand (exit codes, file state, recorded step outputs) and never calls a model.
`Judgment` is effectful and priced: it sends `judgeStatement` plus exactly
the evidence named by `judgeSelect` to the judge backend and gets back a
calibrated probability (see `Question`/`Answer` under (e)).

Evidence-selector mechanism: every executed step records its output in State
under a fresh `StepId` with optional tags, and appends a `TraceEvent` to the
Writer trace; the selector is resolved against that store at evaluation time
(`AllOutputs` = whole trace, `LastN n` = tail, `StepIds` = explicit pins,
`TagMatch t` = tagged subset). Resolution is total: unknown ids/tags resolve
to the empty set and the judgment fails closed (below) rather than throwing.

Cost rationale: checks cost ~0 (no tokens, no latency, deterministic, replay
safe); judgments cost one judge call plus tokens linear in selected evidence.
The selector exists to keep that linear term small and the calibration
meaningful: send the minimal sufficient evidence (pin `StepIds` or a tag,
default `LastN`), because over-broad evidence wastes tokens, adds latency,
and degrades the evaluator's calibration. Tests pin selector-vs-cost
expectations (see (g)).

Thresholds and fail-closed rule: `Reader` config maps each decision to a
confidence threshold; a judgment passes iff calibrated probability >=
threshold. Judge errors, empty evidence resolution, and unparseable answers
all evaluate to "not passed" (boolean contexts) / lowest-confidence answer,
never to an exception that aborts the workflow except via the declared retry
policy.

## (c) RWST execution model

`runWorkflow :: Config -> TrajState -> Workflow a -> IO (a, TrajState, Trace)`.
`RWST Config Trace TrajState IO` conceptually (`mtl`/`transformers` only).

Reader `Config` (static for a run):

- `cfgJudge :: JudgeBackend` — which evaluator answers judgments.
- `cfgThresholds :: Map DecisionId Double` — per-decision pass thresholds for
  judged predicates (missing entry = global default, documented constant).
- `cfgRetry :: RetryPolicy` — `RetryPolicy { maxRetries :: Int, retryOnJudgeError :: Bool }`;
  retries transient judge/action failures only, never re-runs a passed check
  to shop for a different answer.

Writer `Trace` (append-only evidence trace, doubles as judge state):

- `[TraceEvent]`, append-only; interpreters only ever `tell` one event per
  completed step. `TraceEvent = { evStep :: StepId, evKind :: EvKind,
  evSummary :: String, evOutputRef :: StepId }` where `EvKind` is
  `EvAction | EvIntent | EvBranch | EvLoopIter | EvAsk | EvJudgment`.
- The trace IS the evidence pool: selectors resolve against it (+ the State
  output store); the dry-run printer renders it as the plan's step list.

State `TrajState` (live trajectory state):

- `stNextId :: StepId` — fresh-id counter.
- `stOutputs :: Map StepId String` — recorded step outputs (what selectors read).
- `stTags :: Map StepId [String]` — tags for `TagMatch`.
- `stLoops :: Map StepId Int` — per-loop iteration counts (bound enforcement).
- `stPendingAsk :: Maybe StepId` — outstanding user question, if any.

R/W/S separation: config never mutates mid-run; all observations accumulate
in W+S; branching/looping decisions read R+S and append to W.

## (d) Interpreters

1. Live IO executor (`Hydro.Executor`): runs `Act` via `process`/`directory`
   (`Shell` = `readProcessWithExitCode`, reads/writes via `base`/`directory`
   IO), appends one `TraceEvent` + output per step, evaluates `PCheck`
   purely and `PJudge` via `cfgJudge`, enforces `LoopSpec` bounds and
   `cfgRetry`, blocks on `Ask` via stdin/stdout. The only effectful
   interpreter.
2. Dry-run pretty-printer (`Hydro.DryRun`): performs zero effects; renders
   the workflow as a markdown plan (numbered steps, action verbs, predicate
   statements with their selectors and thresholds, loop bounds, ask prompts
   with offered options). Uses `pretty` or plain `String` building (both
   boot libs). Used for review and as golden-test output.
3. Branch-fork semantics (`Hydro.Fork`, speculative execution): `forkWorkflow
   :: TrajState -> Trace -> Workflow a -> [(BranchPath, IO ...)]` conceptually
   forks State+Writer (copy-on-write snapshot of `TrajState` and the trace
   prefix), speculatively runs each `Choice` branch (judged branches by
   pre-evaluated or mock probabilities), then keeps the winner and discards
   the loser's trace suffix; the surviving run's trace records both the
   explored alternatives and the commit decision. No threads in v1 (see (h));
   forks are explored sequentially in a fixed order.

## (e) JudgeBackend interface and Jev

Typed cheap evaluator "Jev": state plus Choice/Score/Noul questions in,
calibrated probabilities out.

```haskell
data Question
  = QChoice [String]   -- pick one option; answer carries per-option probs
  | QScore             -- regression-style pass probability in [0,1]
  | QNoul              -- abstain-aware: pass prob + explicit "no opinion"

data Answer
  = AChoice [(String, Double)]
  | AScore Double
  | ANoul { noulPass :: Double, noulAbstain :: Double }

data JudgeBackend = JudgeBackend
  { jbChoice :: Judgment -> [(String)] -> IO Answer
  , jbScore  :: Judgment -> IO Double
  , jbNoul   :: Judgment -> IO (Double, Double)
  }
```

Backends (both behind this interface, selected by config/env):

- Mock (`Hydro.Judge.Mock`): pure table- or function-driven answers from the
  test or caller (`Map String Double` keyed by statement); default for tests
  and dry runs; returns fixed calibrations, records calls for assertions.
- Env-configured Jev stub (`Hydro.Judge.Jev`): reads `HYDRO_JEV_ENDPOINT`
  (and optional `HYDRO_JEV_API_KEY`, `HYDRO_JEV_TIMEOUT_MS`) at startup; in
  v1 it validates config, serializes the `Question` + selected evidence with
  boot-lib-only code (`base`/`bytestring`), and — with no HTTP client in boot
  libs — shells the request out or returns a typed `JudgeUnavailable` that
  fails closed per (b). Full wire protocol is a non-goal for v1 (see (h)).

## (f) Module breakdown (`Hydro.*`, one line each)

- `Hydro.Workflow` — core `Workflow` ADT plus `Pure`/`Bind` and smart constructors.
- `Hydro.Action` — `Action`/`ActionOutput` types and total output summarizers.
- `Hydro.Predicate` — `CheckSpec`/`Judgment`/`EvidenceSelector` and pure check evaluation.
- `Hydro.Jev.Protocol` — `Question`/`Answer` types and calibration-validating smart constructors.
- `Hydro.Judge` — `JudgeBackend` record interface and fail-closed combinators.
- `Hydro.Judge.Mock` — deterministic table-driven backend for tests and dry runs.
- `Hydro.Judge.Jev` — env-configured Jev stub (`HYDRO_JEV_*`) behind `JudgeBackend`.
- `Hydro.Config` — `Reader` config: backend choice, thresholds, retry policy, env parsing.
- `Hydro.Trace` — `Writer` evidence trace: `TraceEvent` store plus selector resolution.
- `Hydro.State` — `TrajState`: step ids, output/tag/loop-count stores.
- `Hydro.Semantics` — RWST stepping function shared by all interpreters.
- `Hydro.Executor` — live IO interpreter executing actions and predicates.
- `Hydro.DryRun` — effect-free markdown-plan pretty-printer interpreter.
- `Hydro.Fork` — branch-fork snapshots and speculative-execution commit policy.
- `Hydro.Ask` — user-question prompts, option rendering, and answer decoding.

## (g) Test plan (base-only `exitcode-stdio-1.0`, no new deps)

Harness: extend the existing `hydro-test` suite with a small hand-rolled
runner (currently `test/Main.hs` is a placeholder); assertion helper in-test
(`check :: String -> Bool -> IO Bool`) collecting failures and exiting
nonzero on any failure. Areas, each with at least one test:

- Check evaluation: pure truth tables for `ExitZero`/`FileExists`/
  `OutputContains`/`StepSucceeded` and `And`/`Or`/`Not` combinators.
- Selector resolution: `AllOutputs`/`LastN`/`StepIds`/`TagMatch` against a
  hand-built trace, including unknown-id/tag resolving empty and failing closed.
- Thresholds: judgment above/below/equal-to threshold; missing threshold uses default.
- Loop bounds: body that never passes yields `Exhausted n` with exactly
  `maxIters` iterations; `maxIters = 0` rejected by the smart constructor.
- Mock judge: table-driven `Choice`/`Score`/`Noul` answers; call log assertions.
- Jev env parsing: missing/invalid `HYDRO_JEV_*` yields typed config error,
  never an exception; stub failure fails closed.
- Dry-run goldens: representative workflow (action + intent + both choice
  kinds + bounded loop + ask) pretty-prints to a checked-in expected markdown file.
- Fork: both branches explored, winner committed, loser suffix absent from trace.
- RWST laws spot-checks: trace append-only (prefix preserved), fresh ids unique.

## (h) Explicit non-goals for v1

- Real LLM/HTTP wiring for Jev (stub + mock only; no HTTP client dependency).
- Concurrency/parallelism in the executor or fork exploration (sequential only; no `async`).
- Persistence, databases, or resume-from-disk for traces and state.
- Sandboxing, timeouts-as-policy, or resource limits on shell actions beyond `process` basics.
- Rich action set (network, containers, editors); only shell/read/write/edit.
- Learning or adapting thresholds online; thresholds are static config.
- New dependencies of any kind (Hackage or otherwise); new syntax extensions
  beyond what GHC2021 already supplies.
- Changing `MyLib`, `app/Main`, or the test-runner type (`exitcode-stdio-1.0`).

## Open questions / gaps

1. `Ask` transport: stdin/stdout blocking is assumed — is a file- or
   fd-based question channel needed for the harness host?
2. Jev wire format: JSON via boot libs hand-rolled vs shelling to an
   external `curl` — which does the host prefer given no HTTP boot lib?
3. Threshold identity: key thresholds by statement text, by call-site label,
   or by `Question` kind — needs a `DecisionId` decision before implementation.
4. Trace size cap: unbounded `Map StepId String` grows with evidence — do we
   need truncation/GC policy in v1, or is it acceptable?
