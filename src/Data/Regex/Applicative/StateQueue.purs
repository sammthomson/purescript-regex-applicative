-- | This internal module is exposed only for testing and benchmarking. You
-- | don't need to import it.
module Data.Regex.Applicative.StateQueue
  ( StateQueue
  , empty
  , insert
  , insertUnique
  , getElements
  , mkStateQueue
) where

import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.List.Lazy (List, cons, nil, reverse)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as S
import Prelude (class Eq, class Functor, class Show, show, ($), (<<<), (<>))

-- | 'StateQueue' is a data structure that can efficiently insert elements
-- | (preserving their order)
-- | and check whether an element with the given 'Int' key is already in the queue.
newtype StateQueue a = StateQueue {
  elements :: List a,
  ids :: S.Set Int
}

mkStateQueue :: forall a. List a -> S.Set Int -> StateQueue a
mkStateQueue elements ids = StateQueue { elements, ids}

derive instance newtypeStateQueue :: Newtype (StateQueue a) _
derive instance eqStateQueue :: Eq a => Eq (StateQueue a)
instance showStateQueue :: Show a => Show (StateQueue a) where
  show (StateQueue { elements, ids }) =
    "(StateQueue { elements: " <> show elements <> ", " <> show ids <> " })"

derive instance functorStateQueue :: Functor StateQueue

instance foldableStateQueue :: Foldable StateQueue where
  foldr f a = foldr f a <<< getElements
  foldl f a = foldl f a <<< getElements
  foldMap f = foldMap f <<< getElements

-- | Get the list of all elements
getElements :: forall a. StateQueue a -> List a
getElements = reverse <<< _.elements <<< unwrap

-- | The empty state queue
empty :: forall a. StateQueue a
empty = StateQueue {
  elements: nil,
  ids: S.empty
}

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
-- | Since 'insert' doesn't take a key, it won't affect any 'insertUnique'.
insert :: forall a.
          a
          -> StateQueue a
          -> StateQueue a
insert v (StateQueue sq) = StateQueue $
  sq { elements = cons v $ sq.elements }
