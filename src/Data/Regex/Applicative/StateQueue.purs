-- | This internal module is exposed only for testing and benchmarking. You
-- don't need to import it.
module Data.Regex.Applicative.StateQueue
  ( StateQueue
  , empty
  , insert
  , insertUnique
  , getElements
  ) where

import Data.Array (cons, reverse)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as S
import Prelude (class Eq, ($), (<<<))

-- | 'StateQueue' is a data structure that can efficiently insert elements
-- (preserving their order)
-- and check whether an element with the given 'Int' key is already in the queue.
newtype StateQueue a = StateQueue {
  elements :: Array a,
  ids :: S.Set Int
}

derive instance newtypeStateQueue :: Newtype (StateQueue a) _
derive instance eqStateQueue :: Eq a => Eq (StateQueue a)
-- instance showStateQueue :: Show StateQueue where
--   show
  

instance foldableStateQueue :: Foldable StateQueue where
  foldr f a = foldr f a <<< getElements
  foldl f a = foldl f a <<< getElements
  foldMap f = foldMap f <<< getElements

-- | Get the list of all elements
getElements :: forall a. StateQueue a -> Array a
getElements = reverse <<< _.elements <<< unwrap

{-# INLINE empty #-}
-- | The empty state queue
empty :: forall a. StateQueue a
empty = wrap {
  elements: [],
  ids: S.empty
}

{-# INLINE insert #-}
-- | Insert an element in the state queue, unless there is already an element with the same key
insertUnique :: forall a.
                Int -- ^ key
                -> a
                -> StateQueue a
                -> StateQueue a
insertUnique i v sq@StateQueue { ids, elements } =
  if i `S.member` ids
    then sq
    else StateQueue {
      elements: cons v elements,
      ids: S.insert i ids
    }

-- | Insert an element in the state queue without a key.
--
-- Since 'insert' doesn't take a key, it won't affect any 'insertUnique'.
insert :: forall a.
          a
          -> StateQueue a
          -> StateQueue a
insert v (StateQueue sq) = StateQueue $
  sq { elements = cons v $ sq.elements }
