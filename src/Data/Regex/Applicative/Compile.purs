module Data.Regex.Applicative.Compile (
  compile
) where

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.State (State, evalState, modify, runState)
import Data.Array (concatMap)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe', isJust)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Regex.Applicative.Types (Greediness(..), RE, Thread, ThreadId(..), mkThread, runFoldRE, runFoldRE_)
import Data.Tuple (Tuple(..))
import Prelude (class Functor, class Semigroup, const, discard, flip, map, pure, unit, (#), ($), (<$>), (<*>), (<<<), (<>), (>>=), (>>>))


compile :: forall s a r. RE s a -> (a -> Array (Thread s r)) -> Array (Thread s r)
compile e k = unwrap (compile2 e) (SingleCont k)


data Cont a = SingleCont a | EmptyNonEmpty a a

instance functorCont :: Functor Cont where
  map f k =
    case k of
      SingleCont a -> SingleCont (f a)
      EmptyNonEmpty a b -> EmptyNonEmpty (f a) (f b)

emptyCont :: forall a. Cont a -> a
emptyCont k =
  case k of
    SingleCont a -> a
    EmptyNonEmpty a _ -> a

nonEmptyCont :: forall a. Cont a -> a
nonEmptyCont k =
  case k of
    SingleCont a -> a
    EmptyNonEmpty _ a -> a


-- let's help out that poor type inference by making it clear that
-- `ContToArray s r` is a type function of `a`.
newtype ContToArray s r a =
  ContToArray (Cont (a -> Array (Thread s r)) -> Array (Thread s r))
derive instance newtypeContToArray :: Newtype (ContToArray s r a) _

-- The whole point of this module is this function, compile2.
--
-- compile2 function takes two continuations: one when the match is empty and
-- one when the match is non-empty. See the "Rep" case for the reason.
compile2 :: forall s a r. RE s a -> ContToArray s r a
compile2 = runFoldRE {
  eps: wrap \k -> emptyCont k unit,
  symbol: \i p -> wrap \k ->
    let
      t :: (a -> (Array (Thread s r))) -> Thread s r
      t k' = mkThread i $ \s ->
        case p s of
          Just r -> k' r
          Nothing -> []
    in
      [t $ nonEmptyCont k],
  app: \n1 n2 ->
    let
      a1 = unwrap $ compile2 n1
      a2 = unwrap $ compile2 n2
    in
      ContToArray \k -> case k of
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
      a1 = unwrap $ compile2 n1
      a2 = unwrap $ compile2 n2
    in wrap \k -> a1 k <> a2 k,
  fail: wrap $ const [],
  fmap: \f n -> let a = compile2 n in wrap \k -> (unwrap a) $ map ((>>>) f) k,
  -- This is actually the point where we use the difference between
  -- continuations. For the inner RE the empty continuation is a
  -- "failing" one in order to avoid non-termination.
  rep: \g f b n ->
    let
      a = unwrap $ compile2 n
      threads b' k =
        combine
          g
          (a $
            EmptyNonEmpty
              (\_ -> [])
              (\v -> threads (f b' v) (SingleCont $ nonEmptyCont k)))
          (emptyCont k b')
    in wrap $ threads b,
  void: \n ->
    let
      a = compile2_ n
    in
      wrap \k -> a $ map ((#) unit) k
}

data FSMState
  = SAccept
  | STransition ThreadId

type FSMMap s = M.Map Int (Tuple (s -> Boolean) (Array FSMState))

mkNFA :: forall s a. RE s a -> Tuple (Array FSMState) (FSMMap s)
mkNFA e = flip runState M.empty $ go [SAccept] e where
  go :: forall s' a'.
        Array FSMState ->
        RE s' a' ->
        State (FSMMap s') (Array FSMState)
  go k = runFoldRE_ {
    eps: pure k,
    symbol:
      \i@(ThreadId n) p ->
        do
          modify $ M.insert n $ (Tuple (isJust <<< p) k)
          pure [STransition i],
    app: \n1 n2 -> (go k n2) >>= (flip go n1),
    alt: \n1 n2 -> (<>) <$> go k n1 <*> go k n2,
    fail: pure [],
    fmap: \_ n -> go k n,
    rep: \g _ _ n ->
      let
        entries = findEntries n
        cont = combine g entries k
      in
        -- return value of 'go' is ignored -- it should be a subset of
        -- 'cont'
        go cont n >>= \_ -> pure cont,
    void: \n -> go k n
  }

  findEntries :: forall s' a'. RE s' a' -> (Array FSMState)
  findEntries e' =
    -- A simple (although a bit inefficient) way to find all entry points is
    -- just to use 'go'
    evalState (go [] e') M.empty

compile2_ :: forall s a r. RE s a -> Cont (Array (Thread s r)) -> (Array (Thread s r))
compile2_ e' = case mkNFA e' of
  Tuple entries fsmap ->
    let
      mkThread' _ k1 (STransition i@(ThreadId n)) =
        -- TODO: unsafeThrow?!
        case fromMaybe' (\_ -> unsafeThrow "Unknown id") $ M.lookup n fsmap of
          Tuple p cont -> [
            mkThread i $ \s ->
              if p s
                then concatMap (mkThread' k1 k1) cont
                else []
          ]
      mkThread' k0 _ SAccept = k0
    in
      \k -> concatMap (mkThread' (emptyCont k) (nonEmptyCont k)) entries


combine :: forall a. Semigroup a => Greediness -> a -> a -> a
combine Greedy = (<>)
combine NonGreedy = flip (<>)
