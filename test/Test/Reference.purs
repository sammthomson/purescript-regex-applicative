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

module Test.Reference (reference) where
import Control.Alternative (class Alt, class Alternative, empty, map, pure, (<*>), (<|>))
import Control.Applicative (class Applicative, class Apply)
import Control.Monad (class Bind, class Monad, ap, liftM1, (>>=))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Plus (class Plus)
import Data.List.Lazy (List, filter, nil, null, uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Regex.Applicative (Greediness(NonGreedy, Greedy), RE)
import Data.Regex.Applicative.Types (runFoldRE)
import Data.Tuple (Tuple(..), snd)
import Prelude (class Functor, bind, const, flip, unit, ($), (<<<), (<>))


-- A simple parsing monad
newtype P s a = P (List s -> List (Tuple a (List s)))

-- derive newtype instance functorP :: Functor (P s)

instance functorP :: Functor (P s) where
  map = liftM1

instance applyP :: Apply (P s) where
  apply = ap

instance applicativeP :: Applicative (P s) where
  pure x = P $ \s -> (Tuple x s) : nil

instance bindP :: Bind (P s) where
  bind (P a) k =
    P \s -> (a s) >>= (\(Tuple x s') -> case k x of P p -> p s')

instance monadP :: Monad (P s)

instance altP :: Alt (P s) where
  alt (P a1) (P a2) =
    P \s -> a1 s <> a2 s


instance plusP :: Plus (P s) where
  empty = P $ const nil
  
instance alternativeP :: Alternative (P s)

getChar :: forall s. P s s
getChar = P $ \s ->
  case uncons s of
    Nothing -> nil
    Just { head, tail } -> (Tuple head tail : nil)

re2monad :: forall s a. RE s a -> P s a
re2monad = runFoldRE $ {
  eps: P \_ -> unsafeThrow "eps",
  symbol: (\_ p -> do
    c <- getChar
    case p c of
      Just r -> pure r
      Nothing -> empty),
  alt: \a1 a2 -> re2monad a1 <|> re2monad a2,
  app: \a1 a2 -> re2monad a1 <*> re2monad a2,
  fmap: \f a -> map f $ re2monad a,
  rep: \g f b a ->
    let
      am = re2monad a
      rep b' = flip combine (pure b') $ do
        a' <- am
        rep $ f b' a'
      combine a' b' = case g of
        Greedy -> a' <|> b'
        NonGreedy -> b' <|> a'
    in
      rep b,
  void: \a -> re2monad a >>= \_ -> pure unit,
  fail: empty
}

runP :: forall s a. P s a -> List s -> Maybe a
runP (P m) s = case uncons $ filter (null <<< snd) $ m s of
  Just { head: (Tuple r _), tail : _ } -> Just r
  _ -> Nothing

-- | 'reference' @r@ @s@ should give the same results as @s@ '=~' @r@.
--
-- However, this is not very efficient implementation and is supposed to be
-- used for testing only.
reference :: forall s a. RE s a -> List s -> Maybe a
reference r s = runP (re2monad r) s
