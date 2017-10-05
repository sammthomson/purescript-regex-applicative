module Data.Regex.Applicative.Types (
  FoldRE,
  FoldRE_,
  Greediness(..),
  RE,
  Thread(..),
  ThreadId(..),
  mkThread,
  threadId,
  eps,
  symbol,
  alt,
  app,
  fmap,
  fail,
  rep,
  void,
  runFoldRE,
  runFoldRE_
) where

import Data.Maybe

import Control.Alt (class Alt, (<$>))
import Control.Alternative (class Alternative, pure)
import Control.Lazy as Z
import Control.Plus (class Plus)
import Data.Exists (Exists, mkExists, runExists)
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype)
import Prelude (class Applicative, class Apply, class Eq, class Functor, class Ord, Unit, const, unit, ($), (<<<))
import Unsafe.Coerce (unsafeCoerce)

newtype ThreadId = ThreadId (Lazy Int)
instance lazyThreadId :: Z.Lazy ThreadId where
  defer f = ThreadId $ defer \_ -> case f unit of ThreadId i -> force i

-- | A thread either is a result or corresponds to a symbol in the regular
-- expression, which is expected by that thread.
data Thread s r =
  Thread
    { threadId_ :: ThreadId
    , _threadCont :: s -> Array (Thread s r)
    }
  | Accept r

mkThread :: forall s r. ThreadId -> (s -> Array (Thread s r)) -> Thread s r
mkThread i c = Thread { threadId_: i, _threadCont: c }

-- | Returns thread identifier. This will be 'Just' for ordinary threads and
-- 'Nothing' for results.
threadId :: forall s r. Thread s r -> Maybe ThreadId
threadId (Thread { threadId_: i }) = Just i
threadId _ = Nothing

data Greediness = Greedy | NonGreedy

derive instance eqGreediness :: Eq Greediness
derive instance ordGreediness :: Ord Greediness
-- derive instance showGreediness :: Show Greediness
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

instance lazyRE :: Z.Lazy (RE s a) where
  defer f = RE $ defer \_ -> case f unit of RE x -> force x

-- don't export me
re :: forall s a b. RE' s a b -> RE s a
re = RE <<< pure <<< mkExists

-- safe constructors

eps :: forall s. RE s Unit
eps = re Eps

symbol :: forall s a. ThreadId -> (s -> Maybe a) -> RE s a
symbol i f = re $ Symbol i f

alt :: forall s a. RE s a -> RE s a -> RE s a
alt ma mb = re $ Alt ma mb

app :: forall s a b. RE s (b -> a) -> RE s b -> RE s a
app mf mx = re $ App mf mx

fmap :: forall s a b. (b -> a) -> RE s b -> RE s a
fmap f x = re $ Fmap f x

fail :: forall s a. RE s a
fail = re Fail

rep :: forall s a b.
    Greediness       -- repetition may be greedy or not
    -> (a -> b -> a) -- folding function (like in foldl)
    -> a             -- the value for zero matches, and also the initial value
                     -- for the folding function
    -> RE s b
    -> RE s a
rep g op z x = re $ Rep g op z x

void :: forall s a. RE s a -> RE s Unit
void x = re $ Void x

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
  map f x = fmap f x

instance applyRe :: Apply (RE s) where
  apply mf mx = app mf mx

instance applicativeRe :: Applicative (RE s) where
  pure x = const x <$> eps

instance altRe :: Alt (RE s) where
  alt a1 a2 = alt a1 a2

instance plusRe :: Plus (RE s) where
  empty = fail

instance alternativeRe :: Alternative (RE s)
