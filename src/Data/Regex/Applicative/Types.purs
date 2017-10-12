module Data.Regex.Applicative.Types
  ( Greediness(..)
  , Re
  , ThreadId(..)
  , mkApp
  , mkMap
  , mkSymbol
  , mkStar
  , elimRe
) where

import Control.Alt (class Alt, map, (<$>), (<|>))
import Control.Alternative (class Alternative, (<*>))
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Prelude (class Applicative, class Apply, class Eq, class Functor, class Semigroup, class Show, show, (#), ($), (<<<), (<>))


newtype ThreadId = ThreadId Int
derive instance newtypeThreadId :: Newtype ThreadId _

data Greediness = Greedy | NonGreedy

derive instance eqGreediness :: Eq Greediness
derive instance genericGreedines :: Generic Greediness _
instance showGreediness :: Show Greediness where
  show = genericShow

-- | Type of regular expressions that recognize symbols of type `c` and
-- | produce a result of type `a`.
-- |
-- | Regular expressions can be built using `Functor`, `Applicative`,
-- | `Alternative`, and `Monoid` instances in the following natural way:
-- |
-- | * `f <$> ra` matches iff `ra` matches, and its return value is the result
-- | of applying `f` to the return value of `ra`.
-- |
-- | * `pure x` matches the empty string (i.e. it does not consume any symbols),
-- | and its return value is `x`.
-- |
-- | * `rf <*> ra` matches a string iff it is a concatenation of two
-- | strings: one matched by `rf` and the other matched by `ra`. The return value
-- | is `f a`, where `f` and `a` are the return values of `rf` and `ra`
-- | respectively.
-- |
-- | * `ra <> rb` matches a string iff it is a concatenation of two
-- | strings: one matched by `ra` and the other matched by `rb`. The return value
-- | is `a <> b`, where `a` and `b` are the return values of `ra` and `rb`
-- | respectively.
-- |
-- | * `ra <|> rb` matches a string which is accepted by either `ra` or `rb`.
-- | It is left-biased, so if both can match, the result of `ra` is used.
-- |
-- | * `empty` is a regular expression which does not match any string.
-- |
-- | * `many a` matches concatenation of zero or more strings matched by `ra`
-- | and returns the list of `ra`'s return values on those strings.
-- |
-- | * `some ra` matches concatenation of one or more strings matched by `ra`
-- | and returns the list of `ra`'s return values on those strings.
data Re c a =
  Eps a
  | Fail
  | Symbol ThreadId (c -> Maybe a)
  | Alt (Re c a) (Re c a)
  | App (Exists (Apped c a))
  | Star (Exists (Starred c a))
  | Map (Exists (Mapped c a))

-- we don't care about the intermediate type `b`,
-- but it needs to be internally consistent.
data Apped c a b = Apped (Re c (b -> a)) (Re c b)
data Starred c a b = Starred Greediness (a -> b -> a) a (Re c b)
data Mapped c a b = Mapped (b -> a) (Re c b)

mkSymbol :: forall c r. ThreadId -> (c -> Maybe r) -> Re c r
mkSymbol = Symbol

mkApp :: forall c a b. Re c (a -> b) -> Re c a -> Re c b
mkApp rf ra = App $ mkExists $ Apped rf ra

mkMap :: forall c a b. (a -> b) -> Re c a -> Re c b
mkMap f r = Map $ mkExists $ Mapped f r

-- | Match zero or more instances of the given expression, which are combined using
-- | the given folding function.
-- |
-- | `Greediness` argument controls whether this regular expression should match
-- | as many as possible (`Greedy`) or as few as possible (`NonGreedy`) instances
-- | of the underlying expression.
mkStar :: forall c a b.
          Greediness       -- repetition may be greedy or not
          -> (a -> b -> a) -- folding function (like in foldl)
          -> a             -- the value for zero matches, and also the initial value
                           -- for the folding function
          -> Re c b
          -> Re c a
mkStar g op z r = Star $ mkExists $ Starred g op z r

type FoldRe c a r = {
  eps :: a -> r
  , fail :: r
  , symbol :: ThreadId -> (c -> Maybe a) -> r
  , alt :: Re c a -> Re c a -> r
  , app :: forall b. Re c (b -> a) -> Re c b -> r
  , star :: forall b. Greediness -> (a -> b -> a) -> a -> Re c b -> r
  , map :: forall b. (b -> a) -> Re c b -> r
}

-- | Eliminator for Re.
-- | Can take some of the `runExists` cruft out of pattern matching.
elimRe :: forall c a r. FoldRe c a r -> Re c a -> r
elimRe elim re = case re of
  Eps a -> elim.eps a
  Fail -> elim.fail
  Symbol t p -> elim.symbol t p
  Alt a b -> elim.alt a b
  App x -> x # runExists \(Apped ff b) -> elim.app ff b
  Star x -> x # runExists \(Starred g op z re) -> elim.star g op z re
  Map x -> x # runExists \(Mapped f x) -> elim.map f x

instance functorRe :: Functor (Re c) where
  map f = elimRe
    { eps: \a -> Eps $ f a
    , fail: Fail
    , symbol: \t p -> Symbol t (map f <$> p)
    , alt: \a b -> f <$> a <|> f <$> b
    , app: \a b -> (map f) <$> a <*> b
    , star: \g op z r -> mkMap f $ mkStar g op z r
    , map: \g r -> mkMap (f <<< g) r
    }

instance applyRe :: Apply (Re c) where
  apply = mkApp

instance applicativeRe :: Applicative (Re c) where
  pure = Eps

instance altRe :: Alt (Re c) where
  alt = Alt

instance plusRe :: Plus (Re c) where
  empty = Fail

instance alternativeRe :: Alternative (Re c)

instance semigroupRe :: Semigroup a => Semigroup (Re c a) where
  append = lift2 (<>)

instance profunctorRe :: Profunctor Re where
  dimap g f r = f <$> cmap g r where
    -- contravariant map
    cmap :: forall a b c. (a -> b) -> Re b c -> Re a c
    cmap g' = go where
      go :: forall d. Re b d -> Re a d
      go = elimRe {
          eps:              Eps
          , fail:           Fail
          , symbol: \i p    -> Symbol i (p <<< g')  -- <-- g' used
          , alt: \r1 r2     -> go r1 <|> go r2
          , app: \r1 r2     -> go r1 <*> go r2
          , star: \g op z r -> mkStar g op z (go r)
          , map: \f' r     -> f' <$> go r
        }

instance showRe :: Show (Re c a) where
  show = elimRe {
    eps: \_ -> "Eps a?"
    , fail: "Fail"
    , symbol: \_ _ -> "Symbol i? p?"
    , app: \f x -> "App (" <> show f <> ") (" <> show x <> ")"
    , alt: \a b -> "Alt (" <> show a <> ") (" <> show b <> ")"
    , star: \g _ a r -> "Star " <> show g <> " op? a? (" <> show r <> ")"
    , map: \_ b -> "Map f? (" <> show b <> ")"
  }
