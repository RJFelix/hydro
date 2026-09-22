-- | Retry\/threshold policy helpers read from the Reader 'Config'.
--
-- These mirror the live executor's decision rules ('effectiveThreshold'
-- with 'judgmentPasses' decides exactly what the executor decides) and
-- centralize construction-time validation: setters clamp probabilities
-- into @[0,1]@ and retry counts at @0@. 'retryIO' retries only
-- 'Either'-reported failures; callers must catch exceptions into 'Either'
-- first, and the retried action must be idempotent since effects repeat.
module Hydro.Policy
  ( effectiveThreshold
  , judgmentPasses
  , setThreshold
  , setDefaultThreshold
  , setRetryPolicy
  , mkRetryPolicy
  , noRetry
  , shouldRetryJudge
  , retryIO
  ) where

import Control.Exception (IOException, try)
import qualified Data.Map.Strict as Map

import Hydro.Executor (Config (..), RetryPolicy (..), thresholdFor)
import Hydro.Predicate (Judgment (..), clamp01, passJudgment)

-- | Threshold the executor would apply to this judgment: the
-- per-statement entry or the global default.
effectiveThreshold :: Config -> Judgment -> Double
effectiveThreshold config judgment = thresholdFor config (judgeStatement judgment)

-- | Whether a calibrated probability passes under this config's
-- threshold. Equality passes, matching the executor.
judgmentPasses :: Config -> Judgment -> Double -> Bool
judgmentPasses config judgment prob = passJudgment prob (effectiveThreshold config judgment)

-- | Set a per-statement threshold, clamped into @[0,1]@.
setThreshold :: String -> Double -> Config -> Config
setThreshold statement prob config =
  config {cfgThresholds = Map.insert statement (clamp01 prob) (cfgThresholds config)}

-- | Set the global default threshold, clamped into @[0,1]@.
setDefaultThreshold :: Double -> Config -> Config
setDefaultThreshold prob config = config {cfgDefaultThreshold = clamp01 prob}

-- | Replace the retry policy.
setRetryPolicy :: RetryPolicy -> Config -> Config
setRetryPolicy policy config = config {cfgRetry = policy}

-- | Build a retry policy; negative counts clamp to @0@.
mkRetryPolicy :: Int -> Bool -> RetryPolicy
mkRetryPolicy retries onJudgeError = RetryPolicy (max 0 retries) onJudgeError

-- | No retries of any kind.
noRetry :: RetryPolicy
noRetry = RetryPolicy 0 False

-- | Whether judge errors retry under this config: the flag must be set
-- and at least one retry allowed.
shouldRetryJudge :: Config -> Bool
shouldRetryJudge config = retryOnJudgeError (cfgRetry config) && maxRetries (cfgRetry config) > 0

-- | Run an action, retrying 'IOException' failures up to 'maxRetries'
-- times. Non-idempotent actions must not use this: every attempt runs.
retryIO :: RetryPolicy -> IO a -> IO (Either IOException a)
retryIO policy action = go (maxRetries policy)
  where
    go n = do
      attempt <- try action
      case attempt of
        Right x -> pure (Right x)
        Left err -> if n > 0 then go (n - 1) else pure (Left err)
