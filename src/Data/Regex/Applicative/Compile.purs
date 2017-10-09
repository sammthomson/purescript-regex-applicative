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


-- | A thread is either a result, or corresponds to a Symbol in the regular
-- | expression which is expected by that thread.
data Thread c r =
  Thread
    { threadId_ :: ThreadId
    , _threadCont :: c -> List (Thread c r)
    }
  | Accept r

mkThread :: forall c r. ThreadId -> (c -> List (Thread c r)) -> Thread c r
mkThread i c = Thread { threadId_: i, _threadCont: c }

-- | Two continuations: one for when the match is empty, and
-- | one for when the match is non-empty.
-- | This is needed to guarantee termination in the "Star".
data OneOrTwoConts a =
  SingleCont a
  | EmptyNonEmpty a a

instance functorCont :: Functor OneOrTwoConts where
  map f (SingleCont a) = SingleCont (f a)
  map f (EmptyNonEmpty a b) = EmptyNonEmpty (f a) (f b)

emptyCont :: forall a. OneOrTwoConts a -> a
emptyCont (SingleCont a) = a
emptyCont (EmptyNonEmpty a _) = a

nonEmptyCont :: forall a. OneOrTwoConts a -> a
nonEmptyCont (SingleCont a) = a
nonEmptyCont (EmptyNonEmpty _ a) = a

compile :: forall c a r.
           RE c a ->
           (a -> List (Thread c r)) ->
           List (Thread c r)
compile e k = go e (SingleCont k) where
  go :: forall a'. RE c a' ->
                   OneOrTwoConts (a' -> List (Thread c r)) ->
                   List (Thread c r)
  go (Eps a) = flip emptyCont a
  go (Fail) = const nil
  go (Symbol i p) = \k ->
    (mkThread i (p >>> maybe nil (nonEmptyCont k))) : nil
  go (Alt n1 n2) =
    let
      a1 = go n1
      a2 = go n2
    in \k -> a1 k <> a2 k
  go (App r) = runExists (\(Apped n1 n2) ->
    let
      a1 = go n1
      a2 = go n2
      f2 (SingleCont k) = SingleCont $ \aVal -> a2 $ SingleCont $ k <<< aVal
      f2 (EmptyNonEmpty ke kn) =
        let
          ene k1 k2 aVal = a2 $ EmptyNonEmpty (k1 <<< aVal) (k2 <<< aVal)
        in
          EmptyNonEmpty (ene ke kn) (ene kn kn)
    in a1 <<< f2) r
  go (Star r) = runExists (\(Starred g f b n) ->
    let
      a = go n
      combine Greedy = (<>)
      combine NonGreedy = flip (<>)
      threads b' k =
        combine g
          (a $
            EmptyNonEmpty
              (const nil)
              (\v -> threads (f b' v) (SingleCont $ nonEmptyCont k)))
          (emptyCont k b')
    in threads b) r
  go (Fmap r) = runExists (\(Mapped f n) ->
    let a = go n
    in \k -> a $ map ((>>>) f) k
  ) r
