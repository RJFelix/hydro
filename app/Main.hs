module Main (main) where

import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

import Hydro.Action (Action (..), ActionOutput (..))
import Hydro.DryRun (renderPlan)
import Hydro.Executor (defaultConfig, runWorkflow)
import Hydro.Judge (mockBackend)
import Hydro.Predicate (CheckSpec (..), Predicate (..))
import Hydro.State (initState)
import Hydro.Trace (TraceEvent (..))
import Hydro.Workflow (Workflow, act, choice, intent)

-- | Demo workflow: note an intent, echo via shell, branch on the exit code.
demo :: Workflow String
demo = do
  intent "demo run"
  out <- act (Shell "echo" ["hello hydro"])
  choice
    (PCheck (ExitZero out))
    (pure ("saw: " ++ outStdout out))
    (pure "shell failed")

main :: IO ()
main = do
  args <- getArgs
  if "--dry-run" `elem` args
    then putStrLn (renderPlan demo)
    else do
      (result, _, trace) <- runWorkflow (defaultConfig (mockBackend Map.empty)) initState demo
      putStrLn result
      mapM_ (\event -> putStrLn (show (evKind event) ++ ": " ++ evSummary event)) trace
      putStrLn "--- dry-run plan ---"
      putStrLn (renderPlan demo)
