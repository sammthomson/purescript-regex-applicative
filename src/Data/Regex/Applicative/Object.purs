--------------------------------------------------------------------
-- |
-- Module  : Data.Regex.Applicative.Object
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- This is a low-level interface to the regex engine.
--------------------------------------------------------------------
module Data.Regex.Applicative.Object
  ( ReObject
  , compile
  , emptyObject
  , threads
  , failed
  , getResult
  , results
  , step
  , stepThread
  , fromThreads
  , addThread
) where

import Data.Maybe

import Control.Applicative (pure, (<$>), (<*), (<*>))
import Control.Monad.State (State, evalState, gets, modify)
import Data.Foldable (foldl)
import Data.List.Lazy (List, fromFoldable, mapMaybe, nil, null, (:))
import Data.Regex.Applicative.Compile (Thread(..))
import Data.Regex.Applicative.Compile as Compile
import Data.Regex.Applicative.StateQueue as SQ
import Data.Regex.Applicative.Types (RE(..), ThreadId(..), elimRE, mkStar)
import Prelude (flip, ($), (+), (<<<))

-- | The state of the engine is represented as a "regex object" of type
-- | `ReObject c r`, where `c` is the type of symbols and `r` is the
-- | result type (as in the `RE` type). Think of `ReObject` as a collection of
-- | `Thread`s ordered by priority. E.g. threads generated by the left part of
-- | `<|>` come before the threads generated by the right part.
newtype ReObject c r = ReObject (SQ.StateQueue (Thread c r))

-- | List of all threads of an object. Each non-result thread has a unique id.
threads :: forall c r. ReObject c r -> List (Thread c r)
threads (ReObject sq) = fromFoldable sq

-- | Create an object from a list of threads. It is recommended that all
-- | threads come from the same `ReObject`, unless you know what you're doing.
-- | However, it should be safe to filter out or rearrange threads.
fromThreads :: forall c r. List (Thread c r) -> ReObject c r
fromThreads ts = foldl (flip addThread) emptyObject ts

-- | Return the result of a result thread, or `Nothing` if it's not a result
-- | thread
getResult :: forall c r. Thread c r -> Maybe r
getResult (Accept r) = Just r
getResult _ = Nothing

-- | Check if the object has no threads. In that case it never will
-- | produce any new threads as a result of 'step'.
failed :: forall c r. ReObject c r -> Boolean
failed obj = null $ threads obj

-- | Empty object (with no threads)
emptyObject :: forall c r. ReObject c r
emptyObject = ReObject $ SQ.empty

-- | Extract the result values from all the result threads of an object
results :: forall c r. ReObject c r -> List r
results obj = mapMaybe getResult $ threads obj

-- | Feed a symbol into a regex object
step :: forall c r. ReObject c r -> c -> ReObject c r
step (ReObject sq) c = foldl op emptyObject sq where
  op q (Thread t) = foldl (flip addThread) q $ t.threadCont c
  op q (Accept _) = q

-- | Feed a symbol into a thread.
stepThread :: forall c r. c -> Thread c r -> List (Thread c r)
stepThread s (Thread t) = t.threadCont s
stepThread _ _ = nil

-- | Add a thread to an object. The new thread will have lower priority than the
-- | threads which are already in the object.
-- |
-- | If a (non-result) thread with the same id already exists in the object, the
-- | object is not changed.
addThread :: forall c r. Thread c r -> ReObject c r -> ReObject c r
addThread t (ReObject q) =
  ReObject $ case t of
    Accept _ -> SQ.insert t q
    Thread { threadId: ThreadId i } -> SQ.insertUnique i t q

-- | Compile a regular expression into a `ReObject`.
compile :: forall c r. RE c r -> ReObject c r
compile =
  fromThreads <<<
  flip Compile.compile (\x -> Accept x : nil) <<<
  renumber

-- | Give each `Symbol` node a unique `ThreadId`.
renumber :: forall c r. RE c r -> RE c r
renumber e =
  let
    fresh :: State Int ThreadId
    fresh = gets ThreadId <* (modify $ (+) 1)
    go :: forall t b. RE t b -> State Int (RE t b)
    go = elimRE
      { eps: \a -> pure $ Eps a
      , fail: pure Fail
      , symbol: \_ p -> flip Symbol p <$> fresh  -- <-- fresh
      , alt: \a1 a2 -> Alt <$> go a1 <*> go a2
      , app: \a1 a2 -> (<*>) <$> go a1 <*> go a2
      , star: \g f b a -> mkStar g f b <$> go a
      , fmap: \f a -> (<$>) f <$> go a
    }
  in
    evalState (go e) 0
