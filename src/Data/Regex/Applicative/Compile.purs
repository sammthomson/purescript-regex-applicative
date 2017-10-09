module Data.Regex.Applicative.Compile (
  compile
) where

import Control.Monad.State (State, evalState, modify, runState)
import Data.List.Lazy (List, nil, (:))
import Data.Map as M
import Data.Maybe (isJust, maybe)
import Data.Newtype (class Newtype, over)
import Data.Regex.Applicative.Types (Greediness(..), RE, Thread, ThreadId(..), mkThread, elimRE)
import Data.Tuple (Tuple(..), uncurry)
import Prelude (class Functor, const, discard, flip, id, map, pure, ($), (*>), (<$>), (<*>), (<<<), (<>), (>>=), (>>>))


compile :: forall s a r.
           RE s a ->
           (a -> List (Thread s r)) ->
           List (Thread s r)
compile e k = compile2 e (SingleCont k)

-- continuation
data Cont a =
  SingleCont a
  | EmptyNonEmpty a a

instance functorCont :: Functor Cont where
  map f (SingleCont a) = SingleCont (f a)
  map f (EmptyNonEmpty a b) = EmptyNonEmpty (f a) (f b)

emptyCont :: forall a. Cont a -> a
emptyCont (SingleCont a) = a
emptyCont (EmptyNonEmpty a _) = a

nonEmptyCont :: forall a. Cont a -> a
nonEmptyCont (SingleCont a) = a
nonEmptyCont (EmptyNonEmpty _ a) = a

-- The whole point of this module is this function, compile2.
--
-- compile2 function takes two continuations: one when the match is empty and
-- one when the match is non-empty. See the "Rep" case for the reason.
compile2 :: forall s a r.
            RE s a ->
            Cont (a -> List (Thread s r)) ->
            List (Thread s r)
compile2 = elimRE {
  eps: \a k -> emptyCont k a,
  symbol: \i p k ->
    (mkThread i (p >>> maybe nil (nonEmptyCont k))) : nil,
  app: \n1 n2 ->
    let
      a1 = compile2 n1
      a2 = compile2 n2
      f (SingleCont k) =
        SingleCont $ \aVal -> a2 $ SingleCont $ k <<< aVal
      f (EmptyNonEmpty ke kn) =
        let
          ene k1 k2 aVal = a2 $ EmptyNonEmpty (k1 <<< aVal) (k2 <<< aVal)
        in
          EmptyNonEmpty (ene ke kn) (ene kn kn)
    in a1 <<< f,
  alt: \n1 n2 ->
    let
      a1 = compile2 n1
      a2 = compile2 n2
    in \k -> a1 k <> a2 k,
  fail: const nil,
  fmap: \f n ->
    let a = compile2 n
    in \k -> a $ map ((>>>) f) k,
  -- This is the point where we use the difference between
  -- continuations. For the inner RE the empty continuation is a
  -- "failing" one in order to avoid non-termination.
  star: \g f b n ->
    let
      a = compile2 n
      threads b' k =
        combine g (<>)
          (a $
            EmptyNonEmpty
              (\_ -> nil)
              (\v -> threads (f b' v) (SingleCont $ nonEmptyCont k)))
          (emptyCont k b')
    in threads b
}

data Fsa c = Fsa (List FsaState) (FsaMap c)

data FsaState = SAccept | STransition ThreadId

newtype FsaMap c = FsaMap (M.Map Int (Tuple (c -> Boolean) (List FsaState)))
derive instance newtypeFsaMap :: Newtype (FsaMap c) _
emptyFsaMap :: forall c. FsaMap c
emptyFsaMap = FsaMap M.empty

mkFsa :: forall c a. RE c a -> Fsa c
mkFsa e = uncurry Fsa result where
  stateT = go (SAccept : nil) e
  result = runState stateT emptyFsaMap
  go :: forall c' a'.
        List FsaState ->
        RE c' a' ->
        State (FsaMap c') (List FsaState)
  go k = elimRE {
    eps: \a -> pure k  -- FIXME: what to do here?
    , fail: pure nil
    , symbol: \i@(ThreadId n) p -> do
        modify $ over FsaMap $ M.insert n $ Tuple (isJust <<< p) k
        pure ((STransition i) : nil)
    , app: \n1 n2 -> (go k n2) >>= (flip go n1)
    , alt: \n1 n2 -> (<>) <$> go k n1 <*> go k n2
    , star: \g _ _ n ->
      let cont = combine g (<>) (findEntries n) k
      -- return value of 'go' is ignored -- it should be a subset of 'cont's
      in go cont n *> pure cont
    , fmap: \_ n -> go k n
  }

  -- A simple (although a bit inefficient) way to find all entry points is
  -- just to use 'go'
  findEntries :: forall s' a'. RE s' a' -> List FsaState
  findEntries e' = evalState (go nil e') emptyFsaMap


combine :: forall a. Greediness -> (a -> a -> a) -> a -> a -> a
combine Greedy = id
combine NonGreedy = flip
