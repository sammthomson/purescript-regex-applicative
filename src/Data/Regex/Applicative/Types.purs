module Data.Regex.Applicative.Types (
  FoldRE,
  FoldRE_,
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
  mkVoid,
  runFoldRE,
  runFoldRE_
) where

import Data.Maybe

import Control.Alt (class Alt, (<$>))
import Control.Alternative (class Alternative, pure)
import Control.Lazy as Z
import Control.Plus (class Plus)
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lazy (Lazy, force)
import Data.List.Lazy (List)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Prelude (class Applicative, class Apply, class Eq, class Functor, class Ord, class Show, Unit, const, show, ($), (<<<), (<>))
import Unsafe.Coerce (unsafeCoerce)

newtype ThreadId = ThreadId (Lazy Int)
derive newtype instance lazyThreadId :: Z.Lazy ThreadId

-- | A thread either is a result or corresponds to a symbol in the regular
-- expression, which is expected by that thread.
data Thread c r =
  Thread
    { threadId_ :: ThreadId
    , _threadCont :: c -> List (Thread c r)
    }
  | Accept r

mkThread :: forall c r. ThreadId -> (c -> List (Thread c r)) -> Thread c r
mkThread i c = Thread { threadId_: i, _threadCont: c }

-- | Returns thread identifier. This will be 'Just' for ordinary threads and
-- 'Nothing' for results.
threadId :: forall c r. Thread c r -> Maybe ThreadId
threadId (Thread { threadId_: i }) = Just i
threadId _ = Nothing

data Greediness = Greedy | NonGreedy

derive instance eqGreediness :: Eq Greediness
derive instance ordGreediness :: Ord Greediness
derive instance genericGreedines :: Generic Greediness _
instance showGreediness :: Show Greediness where
  show = genericShow

-- | Type of regular expressions that recognize symbols of type @s@ and
-- produce a result of type @a@.
--
-- Regular expressions can be built using 'Functor', 'Applicative' and
-- 'Alternative' instances in the following natural way:
--
-- * @f@ '<$>' @ra@ matches iff @ra@ matches, and its return value is the result
-- of applying @f@ to the return value of @ra@.
--
-- * 'pure' @x@ matches the empty string (i.e. it does not consume any symbols),
-- and its return value is @x@
--
-- * @rf@ '<*>' @ra@ matches a string iff it is a concatenation of two
-- strings: one matched by @rf@ and the other matched by @ra@. The return value
-- is @f a@, where @f@ and @a@ are the return values of @rf@ and @ra@
-- respectively.
--
-- * @ra@ '<|>' @rb@ matches a string which is accepted by either @ra@ or @rb@.
-- It is left-biased, so if both can match, the result of @ra@ is used.
--
-- * 'empty' is a regular expression which does not match any string.
--
-- * 'many' @ra@ matches concatenation of zero or more strings matched by @ra@
-- and returns the list of @ra@'s return values on those strings.
--
-- * 'some' @ra@ matches concatenation of one or more strings matched by @ra@
-- and returns the list of @ra@'s return values on those strings.
data RE' c a b =
  Eps
  | Symbol ThreadId (c -> Maybe a)
  | Alt (RE c a) (RE c a)
  | App (RE c (b -> a)) (RE c b)
  | Fmap (b -> a) (RE c b)
  | Fail
  | Rep Greediness (a -> b -> a) a (RE c b)
  | Void (RE c b)

-- we don't actually care about b
newtype RE c a = RE (Lazy (Exists (RE' c a)))
derive instance newtypeRE :: Newtype (RE c a) _
derive newtype instance lazyRE :: Z.Lazy (RE c a)

instance showRE :: Show (RE c a) where
  show = runFoldRE_ {
    eps: "Eps",
    symbol: \_ _ -> "Symbol threadId? symbolToOuput?",
    app: \f x -> "App (" <> show f <> ") (" <> show x <> ")",
    alt: \a b -> "Alt (" <> show a <> ") (" <> show b <> ")",
    fmap: \_ b -> "Fmap f? (" <> show b <> ")",
    fail: "Fail",
    rep: \g _ a r -> "Rep " <> show g <> " op? a? (" <> show r <> ")",
    void: \_ -> "Void"
  }

-- don't export me
mkRE :: forall c a b. RE' c a b -> RE c a
mkRE = RE <<< pure <<< mkExists

-- safe constructors

mkEps :: forall c. RE c Unit
mkEps = mkRE Eps

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
-- the given folding function.
--
-- 'Greediness' argument controls whether this regular expression should match
-- as many as possible ('Greedy') or as few as possible ('NonGreedy') instances
-- of the underlying expression.
mkRep :: forall c a b.
         Greediness       -- repetition may be greedy or not
         -> (a -> b -> a) -- folding function (like in foldl)
         -> a             -- the value for zero matches, and also the initial value
                          -- for the folding function
         -> RE c b
         -> RE c a
mkRep g op z x = mkRE $ Rep g op z x

mkVoid :: forall c a. RE c a -> RE c Unit
mkVoid x = mkRE $ Void x


-- | 'RE' is a profunctor. This is its contravariant map.
contramapRe :: forall c t a. (t -> c) -> RE c a -> RE t a
contramapRe f = runFoldRE {
    eps:               mkEps,
    symbol: \t p    -> mkSymbol t (p <<< f),
    alt: \r1 r2     -> mkAlt (contramapRe f r1) (contramapRe f r2),
    app: \r1 r2     -> mkApp (contramapRe f r1) (contramapRe f r2),
    fmap: \g r      -> mkFmap g (contramapRe f r),
    fail:              mkFail,
    rep: \gr fn a r -> mkRep gr fn a (contramapRe f r),
    void: \r        -> mkVoid (contramapRe f r)
  }

instance profunctorRe :: Profunctor RE where
  dimap f g r = g <$> contramapRe f r


-- for pattern matching where we want to remember that `a = Unit` for Eps and Void.
type FoldRE c a t = {
  eps :: t Unit,
  symbol :: ThreadId -> (c -> Maybe a) -> t a,
  alt :: RE c a -> RE c a -> t a,
  app :: forall b. RE c (b -> a) -> RE c b -> t a,
  fmap :: forall b. (b -> a) -> RE c b -> t a,
  fail :: t a,
  rep :: forall b. Greediness
          -> (a -> b -> a)
          -> a
          -> RE c b
          -> t a,
  void :: forall b. RE c b -> t Unit
}

runFoldRE' :: forall c a t. FoldRE c a t -> forall b. RE' c a b -> t a
runFoldRE' fld x = case x of
  Eps -> unsafeCoerce fld.eps  -- `a = Unit`, trust us
  Symbol t p -> fld.symbol t p
  Alt a b -> fld.alt a b
  App ff b -> fld.app ff b
  Fmap f x -> fld.fmap f x
  Fail -> fld.fail
  Rep g op z re -> fld.rep g op z re
  Void a -> unsafeCoerce fld.void a  -- `a = Unit`, trust us

runFoldRE :: forall c a t. FoldRE c a t -> RE c a -> t a
runFoldRE fld (RE re) = runExists (runFoldRE' fld) $ force re


-- for pattern matching where we want to forget that `a = Unit` for Eps and Void.
type FoldRE_ c a r = {
  eps :: r,
  symbol :: ThreadId -> (c -> Maybe a) -> r,
  alt :: RE c a -> RE c a -> r,
  app :: forall b. RE c (b -> a) -> RE c b -> r,
  fmap :: forall b. (b -> a) -> RE c b -> r,
  fail :: r,
  rep :: forall b. Greediness
          -> (a -> b -> a)
          -> a
          -> RE c b
          -> r,
  void :: forall b. RE c b -> r
}

runFoldRE_' :: forall c a r. FoldRE_ c a r -> forall b. RE' c a b -> r
runFoldRE_' fld x = case x of
  Eps -> fld.eps
  Symbol t p -> fld.symbol t p
  Alt a b -> fld.alt a b
  App ff b -> fld.app ff b
  Fmap f x -> fld.fmap f x
  Fail -> fld.fail
  Rep g op z re -> fld.rep g op z re
  Void a -> fld.void a

runFoldRE_ :: forall c a r. FoldRE_ c a r -> RE c a -> r
runFoldRE_ fld (RE re) = runExists (runFoldRE_' fld) $ force re

instance functorRe :: Functor (RE c) where
  map f x = mkFmap f x

instance applyRe :: Apply (RE c) where
  apply mf mx = mkApp mf mx

instance applicativeRe :: Applicative (RE c) where
  pure x = const x <$> mkEps

instance altRe :: Alt (RE c) where
  alt a1 a2 = mkAlt a1 a2

instance plusRe :: Plus (RE c) where
  empty = mkFail

instance alternativeRe :: Alternative (RE c)
