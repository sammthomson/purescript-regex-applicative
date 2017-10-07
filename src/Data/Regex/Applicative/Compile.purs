module Data.Regex.Applicative.Compile (
  compile
) where

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.State (State, evalState, modify, runState)
import Data.List.Lazy (List, concatMap, nil, (:))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe', isJust)
import Data.Regex.Applicative.Types (Greediness(..), RE, Thread, ThreadId(..), mkThread, elimRE)
import Data.Tuple (Tuple(..))
import Prelude (class Functor, class Semigroup, const, discard, flip, map, pure, ($), (<$>), (<*>), (<<<), (<>), (>>=), (>>>))


compile :: forall s a r.
           RE s a ->
           (a -> List (Thread s r)) ->
           List (Thread s r)
compile e k = compile2 e (SingleCont k)

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


type ContToList s r a =
  Cont (a -> List (Thread s r)) ->
  List (Thread s r)

-- The whole point of this module is this function, compile2.
--
-- compile2 function takes two continuations: one when the match is empty and
-- one when the match is non-empty. See the "Rep" case for the reason.
compile2 :: forall s a r.
            RE s a ->
            ContToList s r a
compile2 = elimRE {
    eps: \a k -> emptyCont k a,
    symbol: \i p k ->
      let
        t k' = mkThread i $ \s ->
          case p s of
            Just r -> k' r
            Nothing -> nil
      in
        (t $ nonEmptyCont k) : nil,
    app: \n1 n2 ->
      let
        a1 = compile2 n1
        a2 = compile2 n2
      in
        \k -> case k of
          SingleCont k' ->
            a1 $ SingleCont $ \a1_value -> a2 $ SingleCont $ k' <<< a1_value
          EmptyNonEmpty ke kn ->
            a1 $ EmptyNonEmpty
              -- empty
              (\a1_value ->
                a2 $ EmptyNonEmpty (ke <<< a1_value) (kn <<< a1_value))
              -- non-empty
              (\a1_value ->
                a2 $ EmptyNonEmpty (kn <<< a1_value) (kn <<< a1_value)),
    alt: \n1 n2 ->
      let
        a1 = compile2 n1
        a2 = compile2 n2
      in \k -> a1 k <> a2 k,
    fail: const nil,
    fmap: \f n -> let a = compile2 n in \k -> a $ map ((>>>) f) k,
    -- This is actually the point where we use the difference between
    -- continuations. For the inner RE the empty continuation is a
    -- "failing" one in order to avoid non-termination.
    rep: \g f b n ->
      let
        a = compile2 n
        threads b' k =
          combine
            g
            (a $
              EmptyNonEmpty
                (\_ -> nil)
                (\v -> threads (f b' v) (SingleCont $ nonEmptyCont k)))
            (emptyCont k b')
      in threads b
  }

data FSMState
  = SAccept
  | STransition ThreadId

type FSMMap c = M.Map Int (Tuple (c -> Boolean) (List FSMState))

mkNFA :: forall c a.
         RE c a ->
         Tuple (List FSMState) (FSMMap c)
mkNFA e = flip runState M.empty $ go (SAccept : nil) e where
  go :: forall c' a'.
        List FSMState ->
        RE c' a' ->
        State (FSMMap c') (List FSMState)
  go k = elimRE {
    eps: \a -> pure k,  -- FIXME: what to do here?
    symbol:
      \i@(ThreadId n) p ->
        do
          modify $ M.insert n $ Tuple (isJust <<< p) k
          pure (STransition i : nil),
    app: \n1 n2 -> (go k n2) >>= (flip go n1),
    alt: \n1 n2 -> (<>) <$> go k n1 <*> go k n2,
    fail: pure nil,
    fmap: \_ n -> go k n,
    rep: \g _ _ n ->
      let
        entries = findEntries n
        cont = combine g entries k
      in
        -- return value of 'go' is ignored -- it should be a subset of
        -- 'cont'
        go cont n >>= \_ -> pure cont
  }

  -- A simple (although a bit inefficient) way to find all entry points is
  -- just to use 'go'
  findEntries :: forall s' a'. RE s' a' -> (List FSMState)
  findEntries e' = evalState (go nil e') M.empty

compile2_ :: forall s a r.
             RE s a ->
             Cont (List (Thread s r)) ->
             List (Thread s r)
compile2_ e' = case mkNFA e' of
  Tuple entries fsmap ->
    let
      mkThread' _ k1 (STransition i@(ThreadId n)) =
        -- TODO: unsafeThrow?!
        case fromMaybe' (\_ -> unsafeThrow "Unknown id") $ M.lookup n fsmap of
          Tuple p cont -> (
            mkThread i $ \s ->
              if p s
                then concatMap (mkThread' k1 k1) cont
                else nil
          ) : nil
      mkThread' k0 _ SAccept = k0
    in
      \k -> concatMap (mkThread' (emptyCont k) (nonEmptyCont k)) entries


combine :: forall a. Semigroup a => Greediness -> a -> a -> a
combine Greedy = (<>)
combine NonGreedy = flip (<>)
