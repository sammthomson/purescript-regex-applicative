--------------------------------------------------------------------
-- |
-- | Module  : Test.Reference
-- | Copyright : (c) Roman Cheplyaka, 2011; Sam Thomson, 2017.
-- | License   : MIT
-- |
-- | Reference implementation (using backtracking).
--------------------------------------------------------------------
module Test.Reference where

import Control.Alternative (class Alt, class Alternative, empty, pure, (<$>), (<*>), (<|>))
import Control.Applicative (class Applicative, class Apply)
import Control.Monad (class Bind, class Monad, ap, liftM1, (>>=))
import Control.Plus (class Plus)
import Data.Foldable (class Foldable)
import Data.List.Lazy (List, filter, fromFoldable, head, nil, null, uncons, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Regex.Applicative (Re)
import Data.Regex.Applicative.Types (Greediness(..), elimRe)
import Data.Regex.Applicative.SeqLike (class SeqLike, toList)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Functor, bind, const, flip, ($), (<<<), (<>))


-- | A parsing monad. Accepts a List of symbols and returns a List of pairs of
-- | where `a` is the result of a prefix match, and `List c` is the remainder
-- | of the input yet to be matched.
newtype P c a = P (List c -> List (Tuple a (List c)))

instance functorP :: Functor (P c) where
  map = liftM1

-- concat
instance applyP :: Apply (P c) where
  apply = ap

-- eps
instance applicativeP :: Applicative (P c) where
  pure a = P $ \s -> (Tuple a s) : nil

-- concat, where second re can depend on output of first
instance bindP :: Bind (P c) where
  bind (P parse) f =
    P \s -> parse s >>= \(Tuple a rem) -> case f a of P parse' -> parse' rem

instance monadP :: Monad (P c)

-- or
instance altP :: Alt (P c) where
  alt (P a1) (P a2) =
    P \s -> a1 s <> a2 s

-- fail
instance plusP :: Plus (P c) where
  empty = P $ const nil
  
instance alternativeP :: Alternative (P c)

-- | Match and return a single symbol
char :: forall c. P c c
char = P $ \s ->
  case uncons s of
    Nothing -> nil
    Just { head, tail } -> (Tuple head tail : nil)

fromRe :: forall c a. Re c a -> P c a
fromRe = elimRe {
  eps: pure
  , symbol: \_ p -> char >>= \c -> maybe empty pure $ p c
  , alt: \a1 a2 -> fromRe a1 <|> fromRe a2
  , app: \a1 a2 -> fromRe a1 <*> fromRe a2
  , map: \f a -> f <$> fromRe a
  , star: \g f b a ->
    let
      am = fromRe a
      rep b' = flip combine (pure b') $ do
        a' <- am
        rep $ f b' a'
      combine a' b' = case g of
        Greedy -> a' <|> b'
        NonGreedy -> b' <|> a'
    in
      rep b
  , fail: empty
}

runP :: forall c a t. Foldable t => P c a -> t c -> Maybe a
runP (P parse) s =
  let
    fullMatches = filter (null <<< snd) $ parse (fromFoldable s)
  in
    fst <$> head fullMatches

-- | 'reference' @r@ @s@ should give the same results as @s@ '=~' @r@.
-- |
-- | However, this is not a very efficient implementation and is to be
-- | used for testing only.
reference :: forall c s r. SeqLike s c => Re c r -> s -> Maybe r
reference r s = runP (fromRe r) (toList s)
