--------------------------------------------------------------------
-- |
-- | Module    : Data.Regex.Applicative.SeqLike
-- | Copyright : (c) Sam Thomson, 2017
-- | Copyright : (c) Roman Cheplyaka, 2011
-- | License   : MIT
-- |
-- | Typeclass so that `String`, `List Char`, `Array Char`. etc.
-- | can all be matched by regexes.
--------------------------------------------------------------------
module Data.Regex.Applicative.SeqLike where

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.List as L
import Data.List.Lazy as LL
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.String as S
import Data.Unfoldable (class Unfoldable)
import Prelude (class Semigroup, ($), (<<<), (<>))


newtype Seq s = Seq s

derive instance newtypeSeq :: Newtype (Seq s) _

-- | Class for monomorphic sequences
class SeqLike s c | s -> c where
  toSeq :: forall t. Unfoldable t => s -> t c
  fromSeq :: forall t. Foldable t => t c -> s
  length :: s -> Int

empty :: forall s c. SeqLike s c => s
empty = fromSeq []

singleton :: forall s c. SeqLike s c => c -> s
singleton a = fromSeq [a]

toList :: forall s c. SeqLike s c => s -> L.List c
toList = toSeq

fromList :: forall s c. SeqLike s c => L.List c -> s
fromList = fromSeq

instance seqLikeString :: SeqLike String Char where
  toSeq = A.toUnfoldable <<< S.toCharArray
  fromSeq = S.fromCharArray <<< A.fromFoldable
  length = S.length

instance seqLikeArray :: SeqLike (Array a) a where
  toSeq = A.toUnfoldable
  fromSeq = A.fromFoldable
  length = A.length

instance seqLikeList :: SeqLike (L.List a) a where
  toSeq = L.toUnfoldable
  fromSeq = L.fromFoldable
  length = L.length

instance seqLikeLazyList :: SeqLike (LL.List a) a where
  toSeq = LL.toUnfoldable
  fromSeq = LL.fromFoldable
  length = LL.length

instance semigroupSeq :: SeqLike s c => Semigroup (Seq s) where
  append (Seq a) (Seq b) = Seq $ fromList $ toList a <> toList b

instance monoidSeq :: SeqLike s c => Monoid (Seq s) where
  mempty = Seq $ fromSeq []
