--------------------------------------------------------------------
-- |
-- Module  : Text.Regex.Applicative.Reference
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- Reference implementation (using backtracking).
--
-- This is exposed for testing purposes only!
--------------------------------------------------------------------

module Test.Reference where

import Control.Alternative (class Alt, class Alternative, empty, map, pure, (<*>), (<|>))
import Control.Applicative (class Applicative, class Apply)
import Control.Lazy (class Lazy)
import Control.Monad (class Bind, class Monad, ap, liftM1, (>>=))
import Control.Plus (class Plus)
import Data.Foldable (class Foldable)
import Data.List.Lazy (List, filter, fromFoldable, nil, null, uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Regex.Applicative (Greediness(NonGreedy, Greedy), RE)
import Data.Regex.Applicative.Types (runFoldRE)
import Data.Tuple (Tuple(..), snd)
import Debug.Trace (class DebugWarning, traceShow)
import Prelude (class Functor, class Show, Unit, bind, const, flip, unit, ($), (<<<), (<>))


-- | A parsing monad. Accepts a List of symbols and returns a List of pairs of
-- | where `a` is the result of a prefix match, and `List c` is the remainder
-- | of the input yet to be matched.
newtype P c a = P (List c -> List (Tuple a (List c)))

derive newtype instance lazyP :: Lazy (P c a)

instance functorP :: Functor (P c) where
  map = liftM1

instance applyP :: Apply (P c) where
  apply = ap

instance applicativeP :: Applicative (P c) where
  pure x = P $ \s -> (Tuple x s) : nil

instance bindP :: Bind (P c) where
  bind (P a) k =
    P \s -> a s >>= \(Tuple x s') -> case k x of P p -> p s'

instance monadP :: Monad (P c)

instance altP :: Alt (P c) where
  alt (P a1) (P a2) =
    P \s -> a1 s <> a2 s

instance plusP :: Plus (P c) where
  empty = P $ const nil
  
instance alternativeP :: Alternative (P c)

-- | Match and return a single symbol
char :: forall c. P c c
char = P $ \s ->
  case uncons s of
    Nothing -> nil
    Just { head, tail } -> (Tuple head tail : nil)

fromRE :: forall c a. RE c a -> P c a
fromRE = runFoldRE {
  eps: \a -> pure a,
  symbol: (\_ p -> do
    c <- char
    case p c of
      Just r -> pure r
      Nothing -> empty),
  alt: \a1 a2 -> fromRE a1 <|> fromRE a2,
  app: \a1 a2 -> fromRE a1 <*> fromRE a2,
  fmap: \f a -> map f $ fromRE a,
  rep: \g f b a ->
    let
      am = fromRE a
      rep b' = flip combine (pure b') $ do
        a' <- am
        rep $ f b' a'
      combine a' b' = case g of
        Greedy -> a' <|> b'
        NonGreedy -> b' <|> a'
    in
      rep b,
  fail: empty
}

spyShow :: forall a. DebugWarning => Show a => (Unit -> a) -> a
spyShow f = let r = f unit in traceShow r $ pure r

runP :: forall c a t. Foldable t => P c a -> t c -> Maybe a
runP (P m) s = case uncons $ filter (null <<< snd) $ m $ fromFoldable s of
  Just { head: (Tuple r _) } -> Just r
  _ -> Nothing

-- | 'reference' @r@ @s@ should give the same results as @s@ '=~' @r@.
--
-- However, this is not a very efficient implementation and is to be
-- used for testing only.
reference :: forall c a t. Foldable t => RE c a -> t c -> Maybe a
reference r s = runP (fromRE r) s
