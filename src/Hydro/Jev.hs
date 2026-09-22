-- | Jev backend: env-configured typed evaluator behind 'JudgeBackend'.
--
-- Configuration comes from @JEV_API_URL@ (required) and @JEV_API_KEY@
-- (optional). 'JevRequest' carries the statement, the selected evidence,
-- and the typed question; note the 'JudgeBackend' layer receives only the
-- 'Judgment' (evidence pinning is enforced upstream by the executor), so
-- 'jevBackend' sends an empty evidence list until the executor threads
-- evidence through.
--
-- STUB: 'runJevStub' is a clearly-marked stub that always reports
-- 'JevUnavailable'. There is no HTTP client in the boot libraries and no
-- network credentials exist yet, so the stub never touches the network;
-- 'jevBackend' converts the stub failure into an 'IOException', which the
-- executor turns into fail-closed, exactly like 'unavailableBackend'. The
-- table-driven mock backend remains the test path.
module Hydro.Jev
  ( JevConfig (..)
  , JevConfigError (..)
  , JevError (..)
  , JevRequest (..)
  , JevResponse (..)
  , loadJevConfig
  , loadJevConfigFrom
  , runJevStub
  , jevBackend
  , renderJevRequest
  , ppJevConfigSafe
  ) where

import System.Environment (lookupEnv)

import Hydro.Judge (JudgeBackend (..))
import Hydro.Predicate (Answer, Judgment (..), Question, StepId)

-- | Jev connection config. No 'Show' instance is provided on purpose: use
-- 'ppJevConfigSafe' so the API key never lands in logs.
data JevConfig = JevConfig
  { jevApiUrl :: String
  , jevApiKey :: Maybe String
  }
  deriving (Eq)

-- | Typed config-loading failures. Loading never throws; missing or empty
-- @JEV_API_URL@ is a value, not an exception.
data JevConfigError
  = JevMissingUrl
  | JevEmptyUrl
  deriving (Eq, Show)

-- | Typed Jev call failures. 'JevUnavailable' is what the current stub
-- always returns; fail-closed handling maps every case to "not passed".
data JevError
  = JevUnavailable String
  | JevBadConfig JevConfigError
  deriving (Eq, Show)

-- | One evaluator call: free-form claim, pinned evidence, typed question.
-- Carries no credentials by construction.
data JevRequest = JevRequest
  { jevStatement :: String
  , jevEvidence :: [(StepId, String)]
  , jevQuestion :: Question
  }
  deriving (Eq, Show)

-- | One evaluator answer: the calibrated response for the request.
data JevResponse = JevResponse
  { jevAnswer :: Answer
  }
  deriving (Eq, Show)

-- | Pure config validation, shared by 'loadJevConfig' and tests. An empty
-- key counts as unset.
loadJevConfigFrom :: Maybe String -> Maybe String -> Either JevConfigError JevConfig
loadJevConfigFrom Nothing _ = Left JevMissingUrl
loadJevConfigFrom (Just "") _ = Left JevEmptyUrl
loadJevConfigFrom (Just url) key = Right (JevConfig url (nonEmptyKey key))
  where
    nonEmptyKey Nothing = Nothing
    nonEmptyKey (Just "") = Nothing
    nonEmptyKey (Just k) = Just k

-- | Read @JEV_API_URL@\/@JEV_API_KEY@ from the environment.
loadJevConfig :: IO (Either JevConfigError JevConfig)
loadJevConfig = do
  url <- lookupEnv "JEV_API_URL"
  key <- lookupEnv "JEV_API_KEY"
  pure (loadJevConfigFrom url key)

-- | STUB executor: always returns 'JevUnavailable' without touching the
-- network. Replace with the real sender once credentials and a transport
-- exist; keep the 'Left' shape so callers stay fail-closed.
runJevStub :: JevConfig -> JevRequest -> IO (Either JevError JevResponse)
runJevStub _ _ = pure (Left (JevUnavailable stubNote))
  where
    stubNote =
      "Jev stub: no network transport is wired (no HTTP client in boot libs); "
        ++ "set JEV_API_URL/JEV_API_KEY and replace runJevStub with the real sender. "
        ++ "The mock backend remains the test path."

-- | STUB 'JudgeBackend' over the Jev config. Every method fails with an
-- 'IOException' (via the stub), so the executor retries per policy and
-- then fails closed. Evidence is empty here because the backend layer
-- only sees the 'Judgment'; remove this limitation when the executor
-- threads selected evidence through.
jevBackend :: JevConfig -> JudgeBackend
jevBackend config = JudgeBackend choice score noul
  where
    failStub :: Judgment -> IO a
    failStub judgment = do
      result <- runJevStub config (JevRequest (judgeStatement judgment) [] (judgeQuestion judgment))
      case result of
        Right _ -> ioError (userError "Jev stub unexpectedly succeeded")
        Left err -> ioError (userError (show err))
    choice judgment _options = failStub judgment
    score judgment = failStub judgment
    noul judgment = failStub judgment

-- | Human-readable rendering of a request for logs and tests. Hand-rolled
-- with boot-lib-only code; this is NOT the wire format (full protocol is
-- a v1 non-goal). Never includes credentials.
renderJevRequest :: JevRequest -> String
renderJevRequest request =
  unlines
    ( [ "statement: " ++ show (jevStatement request)
      , "question: " ++ show (jevQuestion request)
      , "evidence:"
      ]
        ++ ["  [" ++ show sid ++ "] " ++ show text | (sid, text) <- jevEvidence request]
    )

-- | Log-safe one-line config summary: the key renders as @\<redacted\>@ or
-- @\<unset\>@, never its value.
ppJevConfigSafe :: JevConfig -> String
ppJevConfigSafe config =
  "JevConfig { url = " ++ show (jevApiUrl config) ++ ", key = " ++ keyStatus ++ " }"
  where
    keyStatus = case jevApiKey config of
      Just _ -> "<redacted>"
      Nothing -> "<unset>"
