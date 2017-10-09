module Data.Regex.Applicative.Compile (
  Thread(..)
  , mkThread
  , compile
) where

import Data.Exists (runExists)
import Data.List.Lazy (List, nil, (:))
import Data.Maybe (maybe)
import Data.Regex.Applicative.Types (Apped(..), Greediness(..), Mapped(..), RE(..), Starred(..), ThreadId)
import Prelude (class Functor, const, flip, map, ($), (<<<), (<>), (>>>))


-- | A `Thread` is an intermediate state in the proess of applying a
-- | regex to an input string.
-- | It is either an `Accept` state with a result of type `r`, or a
-- | (non-deterministic) transition function of the next input symbol.
data Thread c r =
  Thread
    { threadId :: ThreadId
    , threadCont :: c -> List (Thread c r)
    }
  | Accept r

mkThread :: forall c r. ThreadId -> (c -> List (Thread c r)) -> Thread c r
mkThread i c = Thread { threadId: i, threadCont: c }

-- | Two continuations: one for when the match is empty, and
-- | one for when the match is non-empty.
-- | We use this to guarantee termination in the "Star" case, so that we don't
-- | match on the empty string forever.
data OneOrTwoConts a =
  OneCont a
  | TwoConts {
    empty :: a
    , nonEmpty :: a
  }
mkTwoConts :: forall a. a -> a -> OneOrTwoConts a
mkTwoConts e n = TwoConts { empty: e, nonEmpty: n }

instance functorCont :: Functor OneOrTwoConts where
  map f (OneCont a) = OneCont (f a)
  map f (TwoConts { empty: a, nonEmpty: b } ) = mkTwoConts (f a) (f b)

-- | Get the appropriate continuation for handling empty matches
emptyCont :: forall a. OneOrTwoConts a -> a
emptyCont (OneCont a) = a
emptyCont (TwoConts t) = t.empty

-- | Get the appropriate continuation for handling non-empty matches
nonEmptyCont :: forall a. OneOrTwoConts a -> a
nonEmptyCont (OneCont a) = a
nonEmptyCont (TwoConts t) = t.nonEmpty

compile :: forall c a r.
           RE c a ->
           (a -> List (Thread c r)) ->
           List (Thread c r)
compile e cont = go e (OneCont cont) where
  go :: forall a'. RE c a' ->
                   OneOrTwoConts (a' -> List (Thread c r)) ->
                   List (Thread c r)
  go (Eps a) = flip emptyCont a  -- empty match, use the empty continuation
  go (Fail) = const nil
  go (Symbol i p) = \k -> (mkThread i (p >>> maybe nil (nonEmptyCont k))) : nil
  go (Alt n1 n2) =
    let
      a1 = go n1
      a2 = go n2
    in \k -> a1 k <> a2 k
  go (App r) = runExists (\(Apped n1 n2) ->
    let
      a1 = go n1
      a2 = go n2
      f2 (OneCont k) = OneCont $ \aVal -> a2 $ OneCont $ k <<< aVal
      f2 (TwoConts { empty: ke, nonEmpty: kn } ) =
        let
          ene k1 k2 aVal = a2 $ mkTwoConts (k1 <<< aVal) (k2 <<< aVal)
        in
          mkTwoConts (ene ke kn) (ene kn kn)
    in a1 <<< f2) r
  go (Star r) = runExists (\(Starred g f b n) ->
    let
      a = go n
      combine Greedy = (<>)
      combine NonGreedy = flip (<>)
      threads b' k =
        combine g
          (a $
            mkTwoConts
              (const nil)
              (\v -> threads (f b' v) (OneCont $ nonEmptyCont k)))
          (emptyCont k b')
    in threads b) r
  go (Fmap r) = runExists (\(Mapped f n) ->
    let a = go n
    in \k -> a $ map ((>>>) f) k
  ) r
