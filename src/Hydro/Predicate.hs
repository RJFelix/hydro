-- | Predicates: deterministic checks versus free-form judgments.
module Hydro.Predicate
  ( StepId
  , Predicate (..)
  , CheckSpec (..)
  , EvidenceSelector (..)
  , Question (..)
  , Answer (..)
  , Judgment (..)
  , checkPaths
  , evalCheck
  , clamp01
  , answerPassProb
  , passJudgment
  ) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map

import Hydro.Action (ActionOutput (..))

-- | Fresh numeric id assigned to every executed step.
type StepId = Int

-- | A branch\/loop condition: either a free deterministic check or a
-- priced free-form judgment sent to the judge backend.
data Predicate
  = PCheck CheckSpec
  | PJudge Judgment
  deriving (Eq, Show)

-- | Pure, total, local, free checks over values already in hand.
--
-- 'StepSucceeded' holds for any recorded step id: the executor only records
-- completed steps, so presence in the output store means the step ran.
-- 'OutputContains' matches when either the stdout or the text payload of
-- the given output contains the needle.
data CheckSpec
  = ExitZero ActionOutput
  | FileExists FilePath
  | OutputContains ActionOutput String
  | StepSucceeded StepId
  | And CheckSpec CheckSpec
  | Or CheckSpec CheckSpec
  | Not CheckSpec
  deriving (Eq, Show)

-- | Names which upstream step outputs travel with a judgment request.
data EvidenceSelector
  = AllOutputs
  | LastN Int
  | StepIds [StepId]
  | TagMatch String
  deriving (Eq, Show)

-- | The Jev typed-question vocabulary: pick-an-option, pass-probability
-- regression, or abstain-aware scoring.
data Question
  = QChoice [String]
  | QScore
  | QNoul
  deriving (Eq, Show)

-- | Calibrated answers from the judge backend.
data Answer
  = AChoice [(String, Double)]
  | AScore Double
  | ANoul { noulPass :: Double, noulAbstain :: Double }
  deriving (Eq, Show)

-- | A free-form claim plus the evidence to send and the question to ask.
data Judgment = Judgment
  { judgeStatement :: String
  , judgeSelect :: EvidenceSelector
  , judgeQuestion :: Question
  } deriving (Eq, Show)

-- | File paths a check depends on (for 'FileExists' IO resolution).
checkPaths :: CheckSpec -> [FilePath]
checkPaths (ExitZero _) = []
checkPaths (FileExists path) = [path]
checkPaths (OutputContains _ _) = []
checkPaths (StepSucceeded _) = []
checkPaths (And left right) = checkPaths left ++ checkPaths right
checkPaths (Or left right) = checkPaths left ++ checkPaths right
checkPaths (Not inner) = checkPaths inner

-- | Total pure evaluation. The file-existence oracle and the recorded
-- output store are passed in so this stays pure; the executor supplies
-- 'System.Directory.doesFileExist' results and 'TrajState' outputs.
evalCheck :: (FilePath -> Bool) -> Map.Map StepId String -> CheckSpec -> Bool
evalCheck _ _ (ExitZero out) = outExit out == Just 0
evalCheck fileExists _ (FileExists path) = fileExists path
evalCheck _ _ (OutputContains out needle) =
  needle `isInfixOf` outStdout out || needle `isInfixOf` outText out
evalCheck _ outputs (StepSucceeded sid) = Map.member sid outputs
evalCheck fileExists outputs (And left right) =
  evalCheck fileExists outputs left && evalCheck fileExists outputs right
evalCheck fileExists outputs (Or left right) =
  evalCheck fileExists outputs left || evalCheck fileExists outputs right
evalCheck fileExists outputs (Not inner) =
  not (evalCheck fileExists outputs inner)

-- | Clamp a raw calibration into @[0,1]@; unparseable answers fail closed
-- downstream via 'passJudgment'.
clamp01 :: Double -> Double
clamp01 x = max 0 (min 1 x)

-- | Boolean-context pass probability of an answer: choice takes the best
-- option's weight, score is direct, Noul drops the abstain mass.
-- Empty choice answers yield @0@ (fail closed).
answerPassProb :: Answer -> Double
answerPassProb (AChoice pairs) = case map snd pairs of
  [] -> 0
  probs -> clamp01 (maximum probs)
answerPassProb (AScore p) = clamp01 p
answerPassProb (ANoul p _) = clamp01 p

-- | A judgment passes iff its calibrated probability meets the threshold;
-- equality passes.
passJudgment :: Double -> Double -> Bool
passJudgment prob threshold = prob >= threshold
