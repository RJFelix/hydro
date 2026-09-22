-- | Dry-run interpreter: renders a workflow as a human-readable markdown
-- plan without executing any action.
--
-- The renderer symbolically executes the monadic structure with stub
-- outputs ('Act' returns a canned success; 'Ask' consumes canned answers,
-- then the spec default, then the first decodable option). Predicates are
-- displayed, never evaluated, so both 'Choice' branches always appear in
-- the plan. Steps after an unanswerable 'Ask' (no default and nothing
-- decodable) cannot be reached symbolically; the plan records a warning
-- and truncates there instead of failing.
{-# LANGUAGE GADTs #-}
module Hydro.DryRun
  ( DryRunOptions (..)
  , defaultDryRunOptions
  , renderPlan
  , renderPlanWith
  ) where

import Data.List (intercalate)

import Hydro.Action (Action (..), ActionOutput (..))
import Hydro.Executor (Config, defaultConfig, thresholdFor)
import Hydro.Judge (unavailableBackend)
import Hydro.Predicate (CheckSpec (..), EvidenceSelector (..), Judgment (..), Predicate (..), Question (..))
import Hydro.Workflow (AskSpec (..), LoopOutcome (..), LoopSpec (..), Workflow (..))

-- | Knobs for the dry-run renderer: thresholds shown next to judgments
-- and canned answers consumed in order by 'Ask' steps.
data DryRunOptions = DryRunOptions
  { dryConfig :: Config
  , dryAskAnswers :: [String]
  }

-- | Defaults: stock thresholds and no canned answers.
defaultDryRunOptions :: DryRunOptions
defaultDryRunOptions = DryRunOptions (defaultConfig (unavailableBackend "dry-run")) []

-- | Render a workflow with default options.
renderPlan :: Workflow a -> String
renderPlan = renderPlanWith defaultDryRunOptions

-- | Render a workflow as markdown without executing any action.
renderPlanWith :: DryRunOptions -> Workflow a -> String
renderPlanWith options workflow = case go workflow fresh of
  (_, final) ->
    unlines
      ( ["# Workflow plan", ""]
          ++ dryLines final
          ++ ["", "(" ++ show (dryNext final - 1) ++ " steps, dry run: no actions were executed.)"]
          ++ map ("Warning: " ++) (reverse (dryWarnings final))
      )
  where
    config = dryConfig options
    fresh = DryS 1 0 [] [] (dryAskAnswers options)

    go :: Workflow b -> DryS -> (Maybe b, DryS)
    go step st = case step of
      Pure x -> (Just x, st)
      Bind left cont -> case go left st of
        (Nothing, st') ->
          ( Nothing
          , warn st' "plan truncated: an earlier step could not be answered in dry-run."
          )
        (Just x, st') -> go (cont x) st'
      Act action -> (Just (stubOutput action), numbered st ("Run " ++ renderAction action))
      Intent note -> (Just (), numbered st ("Note: " ++ note))
      Choice predicate onTrue onFalse ->
        let st1 = numbered st ("Choice on " ++ renderPredicate predicate ++ ":")
            st2 = plain (deeper st1) "Then:"
            (thenVal, st3) = go onTrue st2
            st4 = plain st3 "Else:"
            (elseVal, st5) = go onFalse st4
            st6 = st5 {dryLevel = dryLevel st}
            val = case thenVal of
              Just x -> Just x
              Nothing -> elseVal
         in (val, st6)
      Loop spec predicate body ->
        let st1 =
              numbered
                st
                ( "Loop, up to "
                    ++ show (maxIters spec)
                    ++ " iterations, until "
                    ++ renderPredicate predicate
                    ++ ":"
                )
            (bodyVal, st2) = go body (deeper st1)
         in (fmap Finished bodyVal, st2 {dryLevel = dryLevel st})
      Ask spec ->
        let st1 = numbered st ("Ask: " ++ askPrompt spec)
            st2 = plain st1 ("Options: " ++ intercalate ", " (askOptions spec))
            (answer, st3) = answerAsk st2 spec
         in (answer, st3)

    warn :: DryS -> String -> DryS
    warn st message = st {dryWarnings = message : dryWarnings st}

    numbered :: DryS -> String -> DryS
    numbered st text =
      st
        { dryNext = dryNext st + 1
        , dryLines = dryLines st ++ [replicate (2 * dryLevel st) ' ' ++ show (dryNext st) ++ ". " ++ text]
        }

    plain :: DryS -> String -> DryS
    plain st text = st {dryLines = dryLines st ++ [replicate (2 * dryLevel st) ' ' ++ text]}

    deeper :: DryS -> DryS
    deeper st = st {dryLevel = dryLevel st + 1}

    answerAsk :: DryS -> AskSpec b -> (Maybe b, DryS)
    answerAsk st spec = case takeAnswer (askDecode spec) (dryCanned st) of
      Just (answer, rest) -> (Just answer, st {dryCanned = rest})
      Nothing -> case askDefault spec of
        Just deflt -> (Just deflt, st)
        Nothing -> case takeAnswer (askDecode spec) (askOptions spec) of
          Just (answer, _) -> (Just answer, st)
          Nothing ->
            ( Nothing
            , warn st ("Ask \"" ++ askPrompt spec ++ "\" has no default or decodable answer; plan truncated here.")
            )

    takeAnswer :: (String -> Maybe b) -> [String] -> Maybe (b, [String])
    takeAnswer _ [] = Nothing
    takeAnswer decode (cand : cands) = case decode cand of
      Just answer -> Just (answer, cands)
      Nothing -> case takeAnswer decode cands of
        Just (answer, rest) -> Just (answer, rest)
        Nothing -> Nothing

    renderPredicate :: Predicate -> String
    renderPredicate (PCheck spec) = "check (" ++ renderCheck spec ++ ")"
    renderPredicate (PJudge judgment) =
      "judgment "
        ++ show (judgeStatement judgment)
        ++ " ["
        ++ renderQuestion (judgeQuestion judgment)
        ++ "; evidence: "
        ++ renderSelector (judgeSelect judgment)
        ++ "; threshold: "
        ++ show (thresholdFor config (judgeStatement judgment))
        ++ "]"

-- | Symbolic-execution accumulator: step counter, indent level, plan
-- lines, warnings, and remaining canned 'Ask' answers.
data DryS = DryS
  { dryNext :: Int
  , dryLevel :: Int
  , dryLines :: [String]
  , dryWarnings :: [String]
  , dryCanned :: [String]
  }

-- | Canned success returned for 'Act' during symbolic execution. Never
-- used for branching decisions: predicates are displayed, not evaluated.
stubOutput :: Action -> ActionOutput
stubOutput (Shell _ _) = ActionOutput (Just 0) "<stdout>" ""
stubOutput (ReadFile _) = ActionOutput (Just 0) "" "<file contents>"
stubOutput (WriteFile _ _) = ActionOutput (Just 0) "" "<write acknowledged>"
stubOutput (EditFile _ _ _) = ActionOutput (Just 0) "" "<edit acknowledged>"

-- | One-line rendering of an action.
renderAction :: Action -> String
renderAction (Shell cmd args) = "shell `" ++ unwords (cmd : args) ++ "`"
renderAction (ReadFile path) = "read file " ++ show path
renderAction (WriteFile path contents) =
  "write file " ++ show path ++ " (" ++ show (length contents) ++ " chars)"
renderAction (EditFile path from to) =
  "edit file "
    ++ show path
    ++ " ("
    ++ show (length from)
    ++ " -> "
    ++ show (length to)
    ++ " chars)"

-- | One-line rendering of a deterministic check.
renderCheck :: CheckSpec -> String
renderCheck (ExitZero out) = "exit " ++ maybe "n/a" show (outExit out) ++ " == 0"
renderCheck (FileExists path) = "file exists " ++ show path
renderCheck (OutputContains _ needle) = "output contains " ++ show needle
renderCheck (StepSucceeded sid) = "step " ++ show sid ++ " succeeded"
renderCheck (And left right) = "(" ++ renderCheck left ++ " and " ++ renderCheck right ++ ")"
renderCheck (Or left right) = "(" ++ renderCheck left ++ " or " ++ renderCheck right ++ ")"
renderCheck (Not inner) = "(not " ++ renderCheck inner ++ ")"

-- | One-line rendering of an evidence selector.
renderSelector :: EvidenceSelector -> String
renderSelector AllOutputs = "all outputs"
renderSelector (LastN n) = "last " ++ show n ++ " outputs"
renderSelector (StepIds sids) = "steps " ++ show sids
renderSelector (TagMatch tag) = "tag " ++ show tag

-- | One-line rendering of a judge question.
renderQuestion :: Question -> String
renderQuestion (QChoice options) = "choice among " ++ show options
renderQuestion QScore = "score"
renderQuestion QNoul = "noul (abstain-aware)"
