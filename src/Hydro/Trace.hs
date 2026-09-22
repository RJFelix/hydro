-- | Writer trace: the append-only evidence pool that doubles as judge state.
module Hydro.Trace
  ( EvKind (..)
  , TraceEvent (..)
  , Trace
  , emptyTrace
  , appendEvent
  , resolveEvidence
  ) where

import qualified Data.Map.Strict as Map

import Hydro.Predicate (EvidenceSelector (..), StepId)

-- | What kind of step produced a trace event.
data EvKind
  = EvAction
  | EvIntent
  | EvBranch
  | EvLoopIter
  | EvAsk
  | EvJudgment
  deriving (Eq, Show)

-- | One completed step: its id, kind, human summary, and a reference to
-- its output in the State output store.
data TraceEvent = TraceEvent
  { evStep :: StepId
  , evKind :: EvKind
  , evSummary :: String
  , evOutputRef :: StepId
  } deriving (Eq, Show)

-- | Append-only evidence trace, kept in execution order.
type Trace = [TraceEvent]

-- | The empty trace a run starts from.
emptyTrace :: Trace
emptyTrace = []

-- | Append one event. Interpreters only ever append; prefix is preserved.
appendEvent :: Trace -> TraceEvent -> Trace
appendEvent trace event = trace ++ [event]

-- | Resolve a selector against the trace plus the State output\/tag stores.
-- Total: unknown ids, tags, and non-positive 'LastN' resolve to the empty
-- set so callers can fail closed.
resolveEvidence
  :: EvidenceSelector
  -> Trace
  -> Map.Map StepId String
  -> Map.Map StepId [String]
  -> [(StepId, String)]
resolveEvidence selector trace outputs tags = case selector of
  AllOutputs -> [pair | e <- trace, Just pair <- [outputOf e]]
  LastN n -> drop (length ordered - max 0 n) ordered
    where
      ordered = [pair | e <- trace, Just pair <- [outputOf e]]
  StepIds sids ->
    [(sid, text) | sid <- sids, Just text <- [Map.lookup sid outputs]]
  TagMatch tag ->
    [ pair
    | e <- trace
    , tag `elem` Map.findWithDefault [] (evOutputRef e) tags
    , Just pair <- [outputOf e]
    ]
  where
    outputOf e = fmap (\text -> (evOutputRef e, text)) (Map.lookup (evOutputRef e) outputs)
