-- | Trajectory state: live per-run stores backing ids, outputs, and loops.
module Hydro.State
  ( TrajState (..)
  , initState
  , freshStepId
  , noteStep
  ) where

import qualified Data.Map.Strict as Map

import Hydro.Predicate (StepId)

-- | Live trajectory state (the RWST State component).
data TrajState = TrajState
  { stNextId :: StepId
  , stOutputs :: Map.Map StepId String
  , stTags :: Map.Map StepId [String]
  , stLoops :: Map.Map StepId Int
  , stPendingAsk :: Maybe StepId
  } deriving (Eq, Show)

-- | Fresh state: id counter at zero, all stores empty, no pending question.
initState :: TrajState
initState = TrajState 0 Map.empty Map.empty Map.empty Nothing

-- | Allocate a fresh step id.
freshStepId :: TrajState -> (StepId, TrajState)
freshStepId st = (stNextId st, st {stNextId = stNextId st + 1})

-- | Record a completed step's output text and tags under a fresh id.
noteStep :: String -> [String] -> TrajState -> (StepId, TrajState)
noteStep output stepTags st =
  let (sid, st1) = freshStepId st
   in ( sid
      , st1
          { stOutputs = Map.insert sid output (stOutputs st1)
          , stTags = Map.insert sid stepTags (stTags st1)
          }
      )
