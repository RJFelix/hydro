-- | Judge backends: the injectable evaluator interface, a table-driven
-- mock for tests, and an always-unavailable stub that fails closed.
module Hydro.Judge
  ( JudgeBackend (..)
  , mockBackend
  , mockBackendLogged
  , unavailableBackend
  ) where

import Data.IORef (IORef, modifyIORef')
import qualified Data.Map.Strict as Map

import Hydro.Predicate (Answer (..), Judgment (..))

-- | The Jev evaluator interface: typed questions in, calibrated answers out.
-- Selected by config, so tests inject the mock and production injects Jev.
data JudgeBackend = JudgeBackend
  { jbChoice :: Judgment -> [String] -> IO Answer
  , jbScore :: Judgment -> IO Double
  , jbNoul :: Judgment -> IO (Double, Double)
  }

-- | Pass probability table keyed by statement text. Missing statements
-- answer @0@ so unknown judgments fail closed.
mockLookup :: Map.Map String Double -> Judgment -> Double
mockLookup table judgment =
  Map.findWithDefault 0 (judgeStatement judgment) table

-- | Deterministic table-driven backend. Choice answers put the table weight
-- on the first option and zero elsewhere; Noul answers abstain @0@.
mockBackend :: Map.Map String Double -> JudgeBackend
mockBackend table = JudgeBackend choice score noul
  where
    choice judgment options = pure (AChoice (weight (mockLookup table judgment) options))
    score judgment = pure (mockLookup table judgment)
    noul judgment = pure (mockLookup table judgment, 0)

-- | Mock that additionally logs every judged statement (call order newest
-- first) for call assertions.
mockBackendLogged :: Map.Map String Double -> IORef [String] -> JudgeBackend
mockBackendLogged table logRef = JudgeBackend choice score noul
  where
    choice judgment options = do
      logCall judgment
      pure (AChoice (weight (mockLookup table judgment) options))
    score judgment = do
      logCall judgment
      pure (mockLookup table judgment)
    noul judgment = do
      logCall judgment
      pure (mockLookup table judgment, 0)
    logCall judgment = modifyIORef' logRef (judgeStatement judgment :)

-- | Stub for an unreachable\/unconfigured evaluator: every question raises
-- 'userError', which the executor converts to fail-closed, never an abort.
unavailableBackend :: String -> JudgeBackend
unavailableBackend reason = JudgeBackend choice score noul
  where
    choice _ _ = ioError (userError reason)
    score _ = ioError (userError reason)
    noul _ = ioError (userError reason)

-- | Weight list for mock choice answers.
weight :: Double -> [String] -> [(String, Double)]
weight _ [] = []
weight p (first : rest) = (first, p) : [(option, 0) | option <- rest]
