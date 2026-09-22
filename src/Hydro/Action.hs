-- | Ordinary actions: the agent's effectful vocabulary plus their outputs.
module Hydro.Action
  ( Action (..)
  , ActionOutput (..)
  , summarizeAction
  , summarizeOutput
  ) where

-- | The four ordinary actions from the design: shell commands and file ops.
data Action
  = Shell { shellCmd :: String, shellArgs :: [String] }
  | ReadFile { readPath :: FilePath }
  | WriteFile { writePath :: FilePath, writeContents :: String }
  | EditFile { editPath :: FilePath, editFrom :: String, editTo :: String }
  deriving (Eq, Show)

-- | Result of running an 'Action'.
--
-- Conventions used by the live executor: for 'Shell', 'outStdout' is the
-- child's stdout and 'outText' is the child's stderr; for file actions both
-- payload and diagnostics go in 'outText' and 'outStdout' is empty.
-- 'outExit' is 'Just' @0@ on success, 'Just' non-zero on failure, and
-- 'Nothing' only when no exit notion applies.
data ActionOutput = ActionOutput
  { outExit :: Maybe Int
  , outStdout :: String
  , outText :: String
  } deriving (Eq, Show)

-- | One-line human summary of an action, for trace events and dry runs.
summarizeAction :: Action -> String
summarizeAction (Shell cmd args) = "shell: " ++ unwords (cmd : args)
summarizeAction (ReadFile path) = "read: " ++ path
summarizeAction (WriteFile path contents) =
  "write: " ++ path ++ " (" ++ show (length contents) ++ " chars)"
summarizeAction (EditFile path from to) =
  "edit: " ++ path
    ++ " ("
    ++ show (length from)
    ++ " -> "
    ++ show (length to)
    ++ " chars)"

-- | One-line human summary of an action's output (payloads truncated).
summarizeOutput :: ActionOutput -> String
summarizeOutput out =
  "exit=" ++ maybe "n/a" show (outExit out)
    ++ " stdout="
    ++ show (take 200 (outStdout out))
    ++ " text="
    ++ show (take 200 (outText out))
