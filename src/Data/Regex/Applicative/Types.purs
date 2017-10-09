module Data.Regex.Applicative.Types (
  Greediness(..),
  RE(..),
  ThreadId(..),
  Mapped(..),
  Apped(..),
  Starred(..),
  mkStar,
  elimRE
) where

import Control.Alt (class Alt, (<$>), (<|>))
import Control.Alternative (class Alternative, (<*>))
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Prelude (class Applicative, class Apply, class Eq, class Functor, class Ord, class Semigroup, class Show, show, ($), (<<<), (<>))


newtype ThreadId = ThreadId Int
derive instance newtypeThreadId :: Newtype ThreadId _

data Greediness = Greedy | NonGreedy

derive instance eqGreediness :: Eq Greediness
derive instance ordGreediness :: Ord Greediness
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
data RE c a =
  Eps a
  | Fail
  | Symbol ThreadId (c -> Maybe a)
  | Alt (RE c a) (RE c a)
  | App (Exists (Apped c a))
  | Star (Exists (Starred c a))
  | Fmap (Exists (Mapped c a))

-- we don't actually care about the intermediate type `b`
data Apped c a b = Apped (RE c (b -> a)) (RE c b)
data Starred c a b = Starred Greediness (a -> b -> a) a (RE c b)
data Mapped c a b = Mapped (b -> a) (RE c b)

instance showRE :: Show (RE c a) where
  show = elimRE {
    eps: \_ -> "Eps a?"
    , fail: "Fail"
    , symbol: \_ _ -> "Symbol i? p?"
    , app: \f x -> "App (" <> show f <> ") (" <> show x <> ")"
    , alt: \a b -> "Alt (" <> show a <> ") (" <> show b <> ")"
    , star: \g _ a r -> "Star " <> show g <> " op? a? (" <> show r <> ")"
    , fmap: \_ b -> "Fmap f? (" <> show b <> ")"
  }

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
          -> RE c b
          -> RE c a
mkStar g op z r = Star $ mkExists $ Starred g op z r

-- eliminator for RE
type FoldRE c a r = {
  eps :: a -> r
  , fail :: r
  , symbol :: ThreadId -> (c -> Maybe a) -> r
  , alt :: RE c a -> RE c a -> r
  , app :: forall b. RE c (b -> a) -> RE c b -> r
  , star :: forall b. Greediness -> (a -> b -> a) -> a -> RE c b -> r
  , fmap :: forall b. (b -> a) -> RE c b -> r
}

elimRE :: forall c a r. FoldRE c a r -> RE c a -> r
elimRE elim re = case re of
  Eps a -> elim.eps a
  Fail -> elim.fail
  Symbol t p -> elim.symbol t p
  Alt a b -> elim.alt a b
  App x -> runExists (\(Apped ff b) -> elim.app ff b) x
  Star x -> runExists (\(Starred g op z re) -> elim.star g op z re) x
  Fmap x -> runExists (\(Mapped f x) -> elim.fmap f x) x

instance functorRe :: Functor (RE c) where
  map f x = Fmap $ mkExists $ Mapped f x

instance applyRe :: Apply (RE c) where
  apply mf mx = App $ mkExists $ Apped mf mx

instance applicativeRe :: Applicative (RE c) where
  pure = Eps

instance altRe :: Alt (RE c) where
  alt = Alt

instance plusRe :: Plus (RE c) where
  empty = Fail

instance alternativeRe :: Alternative (RE c)

instance semigroupRe :: Semigroup a => Semigroup (RE c a) where
  append = lift2 (<>)

instance profunctorRe :: Profunctor RE where
  dimap g f r = f <$> cmap g r where
    -- contravariant map
    cmap :: forall a b c. (a -> b) -> RE b c -> RE a c
    cmap g' = go where
      go :: forall d. RE b d -> RE a d
      go = elimRE {
          eps:              Eps
          , fail:           Fail
          , symbol: \i p    -> Symbol i (p <<< g')  -- <-- g' used
          , alt: \r1 r2     -> go r1 <|> go r2
          , app: \r1 r2     -> go r1 <*> go r2
          , star: \g op z r -> mkStar g op z (go r)
          , fmap: \f' r     -> f' <$> go r
        }
