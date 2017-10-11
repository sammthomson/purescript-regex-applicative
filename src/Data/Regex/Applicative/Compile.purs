module Data.Regex.Applicative.Compile
  ( Thread
  , CompiledRe
  , emptyRe
  , compile
  , threads
  , failed
  , getResult
  , results
  , step
  , fromThreads
  , addThread
  ) where

import Control.Applicative (pure, (<*), (<*>))
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.State (State, evalState, gets, modify)
import Control.Plus (empty, (<|>))
import Data.Foldable (foldl)
import Data.List.Lazy (List, fromFoldable, mapMaybe, nil, null, singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Regex.Applicative.StateQueue as SQ
import Data.Regex.Applicative.Types (Greediness(..), RE(Symbol), ThreadId(..), elimRE, mkStar)
import Prelude (class Functor, flip, ($), (+), (<$>), (<<<), (>>>))


-- | A `Thread` is an intermediate state in the process of
-- | applying a regex to an input string.
-- | It is either an `Accept` state with a result of type `r`, or a
-- | (non-deterministic) transition function of the next input symbol.
data Thread c r =
  Thread
    { threadId :: ThreadId
    , step :: c -> CompiledRe c r
    }
  | Accept r

derive instance functorThread :: Functor (Thread c)

-- | Return the result of a thread, or `Nothing` if it's not an `Accept`.
getResult :: forall c r. Thread c r -> Maybe r
getResult (Accept r) = Just r
getResult (Thread _) = Nothing

-- | Feed a symbol into a thread.
stepThread :: forall c r. Thread c r -> c -> CompiledRe c r
stepThread (Thread t) c = t.step c
stepThread (Accept _) _ = emptyRe


-- | A `RE c r` that has been compiled into a non-deterministic state machine.
-- | `c` is the type of input symbols and `r` is the result type.
-- | A `CompiledRe` is implemented as a priority queue of `Thread`s.
-- | Threads generated by the left part of `<|>` have higher
-- | priority than threads generated by the right part, for example.
-- | Each non-result thread has a unique id, corresponding to the Symbol it
-- | is hoping to match next.
newtype CompiledRe c r = CompiledRe (SQ.StateQueue (Thread c r))

derive instance functorCompiledRe :: Functor (CompiledRe c)

emptyRe :: forall c r. CompiledRe c r
emptyRe = CompiledRe SQ.empty

-- | List of all threads of a `CompiledRe`.
threads :: forall c r. CompiledRe c r -> List (Thread c r)
threads (CompiledRe sq) = fromFoldable sq

-- | Create a `CompiledRe` from a list of threads. It is recommended that all
-- | threads come from the same `CompiledRe`, unless you know what you're doing.
-- | However, it should be safe to filter out or rearrange threads.
fromThreads :: forall c r. List (Thread c r) -> CompiledRe c r
fromThreads ts = foldl addThread emptyRe ts

-- | Check if the `CompiledRe` has no threads, in which case it will never match.
failed :: forall c r. CompiledRe c r -> Boolean
failed obj = null $ threads obj

-- | Extract the result values from all the result threads of a `CompiledRe`
results :: forall c r. CompiledRe c r -> List r
results obj = mapMaybe getResult $ threads obj

-- | Feed a symbol into a `CompiledRe`.
step :: forall c r. CompiledRe c r -> c -> CompiledRe c r
step (CompiledRe sq) c = foldl op emptyRe $ sq where
  op acc t = foldl addThread acc $ threads $ stepThread t c

-- | Add a thread to a `CompiledRe`. The new thread will have lower priority than the
-- | threads which are already in the `CompiledRe`.
-- | If a thread with the same id already exists in the queue, the
-- | `CompiledRe` is not changed.
addThread :: forall c r. CompiledRe c r -> Thread c r -> CompiledRe c r
addThread (CompiledRe q) t = CompiledRe $
  case t of
    Accept _ -> SQ.insert t q
    Thread { threadId: ThreadId i } -> SQ.insertUnique i t q

-- | Compile a regular expression into a non-deterministic state machine.
compile :: forall c r. RE c r -> CompiledRe c r
compile e = fromThreads $ runGo (renumber e) accept where
  accept r = singleton (Accept r)
  runGo :: forall a. RE c a -> (a -> List (Thread c r)) -> List (Thread c r)
  runGo r = runContT $ go r
  -- | Builds a `CompiledRe` using continuation passing style.
  -- | Given a regex `re` and continuation `k`, `go` compiles `r` into a list
  -- | of threads, but replaces `Accept r` states with `k r`.
  go :: forall a. RE c a -> ContT (Thread c r) List a
  go = elimRE
    { eps: pure
    , fail: ContT \_ -> nil
    , symbol: \i p ->
        ContT \k -> singleton $ Thread
          { threadId: i
          , step: p >>> maybe emptyRe (fromThreads <<< k)
          }
    , alt: \r1 r2 -> ContT $ (<|>) <$> runGo r1 <*> runGo r2  -- uses applyFn
    , app: \r1 r2 -> go r1 <*> go r2
    , star: \g op z r ->
        let
          rAndThen = runGo r
          or = case g of
            Greedy -> flip (<|>)
            NonGreedy -> (<|>)
          -- either eps or `r r*`, Greedy prefers the latter.
          star a k = k a `or` rAndThen \b -> star (a `op` b) k
        in
          ContT $ star z
    , fmap: \f r -> f <$> go r }

-- | Give each `Symbol` node a unique `ThreadId`.
renumber :: forall c r. RE c r -> RE c r
renumber e =
  let
    fresh :: State Int ThreadId
    fresh = gets ThreadId <* (modify $ (+) 1)
    go :: forall t b. RE t b -> State Int (RE t b)
    go = elimRE
      { eps: \a -> pure $ pure a
      , fail: pure empty
      , symbol: \_ p -> flip Symbol p <$> fresh  -- <-- fresh
      , alt: \a1 a2 -> (<|>) <$> go a1 <*> go a2
      , app: \a1 a2 -> (<*>) <$> go a1 <*> go a2
      , star: \g f b a -> mkStar g f b <$> go a
      , fmap: \f a -> (<$>) f <$> go a
    }
  in
    evalState (go e) 0
