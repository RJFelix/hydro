{-# LANGUAGE GADTs #-}

-- | Core workflow ADT: one constructor per agent-harness construct, with
-- 'Pure'\/'Bind' plumbing so ad-hoc workflows sequence with do-notation.
module Hydro.Workflow
  ( Workflow (..)
  , LoopSpec (..)
  , LoopOutcome (..)
  , AskSpec (..)
  , mkLoopSpec
  , mkChoiceAsk
  , act
  , intent
  , choice
  , loop
  , ask
  ) where

import Hydro.Action (Action, ActionOutput)
import Hydro.Predicate (Predicate)

-- | Bound on loop iterations. Mandatory so every loop terminates even when
-- its condition never passes.
newtype LoopSpec = LoopSpec {maxIters :: Int}
  deriving (Eq, Show)

-- | How a loop ended: the passing body result, or the iteration count when
-- the bound was hit.
data LoopOutcome a
  = Finished a
  | Exhausted Int
  deriving (Eq, Show)

-- | A user question: prompt, offered options, a decoder from the user's
-- reply line, and an optional default. 'Just' a default makes 'Ask'
-- non-interactive (used by tests); 'Nothing' blocks on stdin\/stdout.
data AskSpec a = AskSpec
  { askPrompt :: String
  , askOptions :: [String]
  , askDecode :: String -> Maybe a
  , askDefault :: Maybe a
  }

-- | The workflow DSL. 'Act' runs ordinary actions, 'Intent' records notes,
-- 'Choice' branches on a check or judgment with one continued workflow per
-- branch, 'Loop' repeats its body until the predicate passes or the bound
-- hits, and 'Ask' solicits user input.
data Workflow a where
  Pure :: a -> Workflow a
  Bind :: Workflow a -> (a -> Workflow b) -> Workflow b
  Act :: Action -> Workflow ActionOutput
  Intent :: String -> Workflow ()
  Choice :: Predicate -> Workflow a -> Workflow a -> Workflow a
  Loop :: LoopSpec -> Predicate -> Workflow a -> Workflow (LoopOutcome a)
  Ask :: AskSpec a -> Workflow a

instance Functor Workflow where
  fmap f workflow = Bind workflow (Pure . f)

instance Applicative Workflow where
  pure = Pure
  wf <*> wx = Bind wf (\f -> Bind wx (\x -> Pure (f x)))

instance Monad Workflow where
  (>>=) = Bind

-- | Smart constructor: 'Nothing' for bounds below @1@, enforcing the
-- mandatory-bound rule at construction time.
mkLoopSpec :: Int -> Maybe LoopSpec
mkLoopSpec n
  | n >= 1 = Just (LoopSpec n)
  | otherwise = Nothing

-- | Build an option-picking question from @(option, value)@ pairs; the
-- decoder matches the user's exact reply line against an option.
mkChoiceAsk :: String -> [(String, a)] -> Maybe a -> AskSpec a
mkChoiceAsk prompt options deflt =
  AskSpec prompt (map fst options) (`lookup` options) deflt

-- | Run an ordinary action.
act :: Action -> Workflow ActionOutput
act = Act

-- | Record a note to future-self or other agents.
intent :: String -> Workflow ()
intent = Intent

-- | Branch on a deterministic check or free-form judgment.
choice :: Predicate -> Workflow a -> Workflow a -> Workflow a
choice = Choice

-- | Repeat the body until the predicate passes or the bound hits.
loop :: LoopSpec -> Predicate -> Workflow a -> Workflow (LoopOutcome a)
loop = Loop

-- | Solicit user input or offer a user decision.
ask :: AskSpec a -> Workflow a
ask = Ask
