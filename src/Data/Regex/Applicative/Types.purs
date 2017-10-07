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

import Control.Alt (class Alt, (<$>))
import Control.Alternative (class Alternative, pure)
import Control.Lazy as Z
import Control.Plus (class Plus)
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lazy (Lazy, force)
import Data.Maybe
import Data.List.Lazy (List)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Prelude (class Applicative, class Apply, class Eq, class Functor, class Ord, class Show, Unit, const, show, ($), (<<<), (<>))
import Unsafe.Coerce (unsafeCoerce)

newtype ThreadId = ThreadId (Lazy Int)
derive newtype instance lazyThreadId :: Z.Lazy ThreadId

-- | A thread either is a result or corresponds to a symbol in the regular
-- expression, which is expected by that thread.
data Thread s r =
  Thread
    { threadId_ :: ThreadId
    , _threadCont :: s -> List (Thread s r)
    }
  | Accept r

mkThread :: forall s r. ThreadId -> (s -> List (Thread s r)) -> Thread s r
mkThread i c = Thread { threadId_: i, _threadCont: c }

-- | Returns thread identifier. This will be 'Just' for ordinary threads and
-- 'Nothing' for results.
threadId :: forall s r. Thread s r -> Maybe ThreadId
threadId (Thread { threadId_: i }) = Just i
threadId _ = Nothing

data Greediness = Greedy | NonGreedy

derive instance eqGreediness :: Eq Greediness
derive instance ordGreediness :: Ord Greediness
derive instance genericGreedines :: Generic Greediness _
instance showGreediness :: Show Greediness where
  show = genericShow
-- derive instance readGreediness :: Read Greediness
-- derive instance enumGreediness :: Enum Greediness

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
data RE' s a b =
  Eps
  | Symbol ThreadId (s -> Maybe a)
  | Alt (RE s a) (RE s a)
  | App (RE s (b -> a)) (RE s b)
  | Fmap (b -> a) (RE s b)
  | Fail
  | Rep Greediness (a -> b -> a) a (RE s b)
  | Void (RE s b)

-- we don't actually care about b
newtype RE s a = RE (Lazy (Exists (RE' s a)))
derive instance newtypeRE :: Newtype (RE s a) _
derive newtype instance lazyRE :: Z.Lazy (RE s a)

instance showRE :: Show (RE s a) where
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
mkRE :: forall s a b. RE' s a b -> RE s a
mkRE = RE <<< pure <<< mkExists

-- safe constructors

mkEps :: forall s. RE s Unit
mkEps = mkRE Eps

mkSymbol :: forall s a. ThreadId -> (s -> Maybe a) -> RE s a
mkSymbol i f = mkRE $ Symbol i f

mkAlt :: forall s a. RE s a -> RE s a -> RE s a
mkAlt ma mb = mkRE $ Alt ma mb

mkApp :: forall s a b. RE s (b -> a) -> RE s b -> RE s a
mkApp mf mx = mkRE $ App mf mx

mkFmap :: forall s a b. (b -> a) -> RE s b -> RE s a
mkFmap f x = mkRE $ Fmap f x

mkFail :: forall s a. RE s a
mkFail = mkRE Fail

mkRep :: forall s a b.
         Greediness       -- repetition may be greedy or not
         -> (a -> b -> a) -- folding function (like in foldl)
         -> a             -- the value for zero matches, and also the initial value
                          -- for the folding function
         -> RE s b
         -> RE s a
mkRep g op z x = mkRE $ Rep g op z x

mkVoid :: forall s a. RE s a -> RE s Unit
mkVoid x = mkRE $ Void x


-- -- | 'RE' is a profunctor. This is its contravariant map.
-- comapRe :: forall s1 s2 a1 a2. (s2 -> s1) -> (a1 -> a2) -> RE s1 a1 -> RE s2 a2
-- comapRe f = runFoldRE {
--     eps:               mkEps,
--     symbol: \t p    -> mkSymbol t (p <<< f),
--     alt: \r1 r2     -> mkAlt (comapRe f r1) (comapRe f r2),
--     app: \r1 r2     -> mkApp (comapRe f r1) (comapRe f r2),
--     fmap: \g r      -> mkFmap g (comapRe f r),
--     fail:              mkFail,
--     rep: \gr fn a r -> mkRep gr fn a (comapRe f r),
--     void: \r        -> mkVoid (comapRe f r)
--   }

-- instance profunctorRe :: Profunctor (RE s a) where
--   dimap = comapRe

-- for pattern matching where we want to remember that `a = Unit` for Eps and Void.
type FoldRE s a t = {
  eps :: t Unit,
  symbol :: ThreadId -> (s -> Maybe a) -> t a,
  alt :: RE s a -> RE s a -> t a,
  app :: forall b. RE s (b -> a) -> RE s b -> t a,
  fmap :: forall b. (b -> a) -> RE s b -> t a,
  fail :: t a,
  rep :: forall b. Greediness
          -> (a -> b -> a)
          -> a
          -> RE s b
          -> t a,
  void :: forall b. RE s b -> t Unit
}

runFoldRE' :: forall s a t. FoldRE s a t -> forall b. RE' s a b -> t a
runFoldRE' fld x = case x of
  Eps -> unsafeCoerce fld.eps  -- `a = Unit`, trust us
  Symbol t p -> fld.symbol t p
  Alt a b -> fld.alt a b
  App ff b -> fld.app ff b
  Fmap f x -> fld.fmap f x
  Fail -> fld.fail
  Rep g op z re -> fld.rep g op z re
  Void a -> unsafeCoerce fld.void a  -- `a = Unit`, trust us

runFoldRE :: forall s a t. FoldRE s a t -> RE s a -> t a
runFoldRE fld (RE re) = runExists (runFoldRE' fld) $ force re


-- for pattern matching where we want to forget that `a = Unit` for Eps and Void.
type FoldRE_ s a r = {
  eps :: r,
  symbol :: ThreadId -> (s -> Maybe a) -> r,
  alt :: RE s a -> RE s a -> r,
  app :: forall b. RE s (b -> a) -> RE s b -> r,
  fmap :: forall b. (b -> a) -> RE s b -> r,
  fail :: r,
  rep :: forall b. Greediness
          -> (a -> b -> a)
          -> a
          -> RE s b
          -> r,
  void :: forall b. RE s b -> r
}

runFoldRE_' :: forall s a r. FoldRE_ s a r -> forall b. RE' s a b -> r
runFoldRE_' fld x = case x of
  Eps -> fld.eps
  Symbol t p -> fld.symbol t p
  Alt a b -> fld.alt a b
  App ff b -> fld.app ff b
  Fmap f x -> fld.fmap f x
  Fail -> fld.fail
  Rep g op z re -> fld.rep g op z re
  Void a -> fld.void a

runFoldRE_ :: forall s a r. FoldRE_ s a r -> RE s a -> r
runFoldRE_ fld (RE re) = runExists (runFoldRE_' fld) $ force re

instance functorRe :: Functor (RE s) where
  map f x = mkFmap f x

instance applyRe :: Apply (RE s) where
  apply mf mx = mkApp mf mx

instance applicativeRe :: Applicative (RE s) where
  pure x = const x <$> mkEps

instance altRe :: Alt (RE s) where
  alt a1 a2 = mkAlt a1 a2

instance plusRe :: Plus (RE s) where
  empty = mkFail

instance alternativeRe :: Alternative (RE s)
