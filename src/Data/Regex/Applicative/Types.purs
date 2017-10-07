module Data.Regex.Applicative.Types (
  Greediness(..),
  RE,
  Thread(..),
  ThreadId(..),
  mkThread,
  threadId,
  mkEps,
  mkSymbol,
  mkAlt,
  mkApp,
  mkFmap,
  mkFail,
  mkRep,
  elimRE
) where

import Data.Maybe

import Control.Alt (class Alt, (<$>))
import Control.Alternative (class Alternative, (<*>))
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (List)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Prelude (class Applicative, class Apply, class Eq, class Functor, class Ord, class Semigroup, class Show, show, ($), (<<<), (<>))


newtype ThreadId = ThreadId Int
derive instance newtypeThreadId :: Newtype ThreadId _

-- | A thread either is a result or corresponds to a symbol in the regular
-- | expression, which is expected by that thread.
data Thread c r =
  Thread
    { threadId_ :: ThreadId
    , _threadCont :: c -> List (Thread c r)
    }
  | Accept r

mkThread :: forall c r. ThreadId -> (c -> List (Thread c r)) -> Thread c r
mkThread i c = Thread { threadId_: i, _threadCont: c }

-- | Returns thread identifier. This will be 'Just' for ordinary threads and
-- | 'Nothing' for results.
threadId :: forall c r. Thread c r -> Maybe ThreadId
threadId (Thread { threadId_: i }) = Just i
threadId _ = Nothing

data Greediness = Greedy | NonGreedy

derive instance eqGreediness :: Eq Greediness
derive instance ordGreediness :: Ord Greediness
derive instance genericGreedines :: Generic Greediness _
instance showGreediness :: Show Greediness where
  show = genericShow

-- | Type of regular expressions that recognize symbols of type @c@ and
-- | produce a result of type @a@.
-- |
-- | Regular expressions can be built using 'Functor', 'Applicative' and
-- | 'Alternative' instances in the following natural way:
-- |
-- | * @f@ '<$>' @ra@ matches iff @ra@ matches, and its return value is the result
-- | of applying @f@ to the return value of @ra@.
-- |
-- | * 'pure' @x@ matches the empty string (i.e. it does not consume any symbols),
-- | and its return value is @x@
-- |
-- | * @rf@ '<*>' @ra@ matches a string iff it is a concatenation of two
-- | strings: one matched by @rf@ and the other matched by @ra@. The return value
-- | is @f a@, where @f@ and @a@ are the return values of @rf@ and @ra@
-- | respectively.
-- |
-- | * @ra@ '<|>' @rb@ matches a string which is accepted by either @ra@ or @rb@.
-- | It is left-biased, so if both can match, the result of @ra@ is used.
-- |
-- | * 'empty' is a regular expression which does not match any string.
-- |
-- | * 'many' @ra@ matches concatenation of zero or more strings matched by @ra@
-- | and returns the list of @ra@'s return values on those strings.
-- |
-- | * 'some' @ra@ matches concatenation of one or more strings matched by @ra@
-- | and returns the list of @ra@'s return values on those strings.
data RE' c a b =
  Eps a
  | Fail
  | Symbol ThreadId (c -> Maybe a)
  | Alt (RE c a) (RE c a)
  | App (RE c (b -> a)) (RE c b)
  | Fmap (b -> a) (RE c b)
  | Rep Greediness (a -> b -> a) a (RE c b)

-- we don't actually care about b
newtype RE c a = RE (Exists (RE' c a))

instance showRE :: Show (RE c a) where
  show = elimRE {
    eps: \_ -> "Eps ?",
    fail: "Fail",
    symbol: \_ _ -> "Symbol threadId? symbolToOuput?",
    app: \f x -> "App (" <> show f <> ") (" <> show x <> ")",
    alt: \a b -> "Alt (" <> show a <> ") (" <> show b <> ")",
    fmap: \_ b -> "Fmap f? (" <> show b <> ")",
    rep: \g _ a r -> "Rep " <> show g <> " op? a? (" <> show r <> ")"
  }

-- don't export me
mkRE :: forall c a b. RE' c a b -> RE c a
mkRE = RE <<< mkExists

-- constructors

mkEps :: forall c a. a -> RE c a
mkEps a = mkRE $ Eps a

mkSymbol :: forall c a. ThreadId -> (c -> Maybe a) -> RE c a
mkSymbol i f = mkRE $ Symbol i f

mkAlt :: forall c a. RE c a -> RE c a -> RE c a
mkAlt ma mb = mkRE $ Alt ma mb

mkApp :: forall c a b. RE c (b -> a) -> RE c b -> RE c a
mkApp mf mx = mkRE $ App mf mx

mkFmap :: forall c a b. (b -> a) -> RE c b -> RE c a
mkFmap f x = mkRE $ Fmap f x

mkFail :: forall c a. RE c a
mkFail = mkRE Fail

-- | Match zero or more instances of the given expression, which are combined using
-- | the given folding function.
-- |
-- | 'Greediness' argument controls whether this regular expression should match
-- | as many as possible ('Greedy') or as few as possible ('NonGreedy') instances
-- | of the underlying expression.
mkRep :: forall c a b.
         Greediness       -- repetition may be greedy or not
         -> (a -> b -> a) -- folding function (like in foldl)
         -> a             -- the value for zero matches, and also the initial value
                          -- for the folding function
         -> RE c b
         -> RE c a
mkRep g op z r = mkRE $ Rep g op z r

-- | 'RE' is a profunctor. This is its contravariant map.
contramapRe :: forall c t a. (t -> c) -> RE c a -> RE t a
contramapRe f = go where
  go :: forall a'. RE c a' -> RE t a'
  go = elimRE {
      eps:               mkEps,
      symbol: \i p    -> mkSymbol i (p <<< f),  -- <- only place input is consumed
      alt: \r1 r2     -> mkAlt (go r1) (go r2),
      app: \r1 r2     -> mkApp (go r1) (go r2),
      fmap: \g r      -> mkFmap g (go r),
      fail:              mkFail,
      rep: \g op z r -> mkRep g op z (go r)
    }

instance profunctorRe :: Profunctor RE where
  dimap f g r = g <$> contramapRe f r


-- eliminator for RE
type FoldRE c a r = {
  eps :: a -> r,
  symbol :: ThreadId -> (c -> Maybe a) -> r,
  alt :: RE c a -> RE c a -> r,
  app :: forall b. RE c (b -> a) -> RE c b -> r,
  fmap :: forall b. (b -> a) -> RE c b -> r,
  fail :: r,
  rep :: forall b. Greediness -> (a -> b -> a) -> a -> RE c b -> r
}

elimRE :: forall c a r. FoldRE c a r -> RE c a -> r
elimRE elim (RE re) = runExists go re where
  go :: forall b. RE' c a b -> r
  go x = case x of
    Eps a -> elim.eps a
    Symbol t p -> elim.symbol t p
    Alt a b -> elim.alt a b
    App ff b -> elim.app ff b
    Fmap f x -> elim.fmap f x
    Fail -> elim.fail
    Rep g op z re -> elim.rep g op z re


instance functorRe :: Functor (RE c) where
  map = mkFmap

instance applyRe :: Apply (RE c) where
  apply = mkApp

instance applicativeRe :: Applicative (RE c) where
  pure = mkEps

instance altRe :: Alt (RE c) where
  alt = mkAlt

instance plusRe :: Plus (RE c) where
  empty = mkFail

instance alternativeRe :: Alternative (RE c)

instance semigroupRe :: Semigroup a => Semigroup (RE c a) where
  append = lift2 (<>)
