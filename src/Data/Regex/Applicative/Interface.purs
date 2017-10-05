module Data.Regex.Applicative.Interface where

import Control.Alternative (class Alt, class Alternative, class Plus, (<$>), (<|>))
import Control.Lazy (defer)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (cons, foldl, head, init, reverse, uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (second, (***))
import Data.Regex.Applicative.Object (addThread, compile, emptyObject, failed, fromThreads, getResult, results, step, threads)
import Data.Regex.Applicative.Types (Greediness(..), RE, Thread(..), alt, app, eps, fail, fmap, rep, runFoldRE, symbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prelude (class Applicative, class Apply, class Eq, class Functor, Ordering(..), compare, const, flip, id, map, not, unit, ($), (#), (&&), (+), (<$>), (<*>), (<<<), (<>), (==), (>>>))



  -- empty = Fail
  -- many a = reverse <$> Rep Greedy (flip (:)) [] a
  -- some a = (:) <$> a <*> many a

-- instance (char ~ Char, string ~ String) => IsString (RE char string) where
--   fromString = string

-- | 'RE' is a profunctor. This is its contravariant map.
--
-- (A dependency on the @profunctors@ package doesn't seem justified.)
-- comap :: (s2 -> s1) -> RE s1 a -> RE s2 a
-- comap f re =
--   case re of
--     Eps -> Eps
--     Symbol t p    -> Symbol t (p . f)
--     Alt r1 r2     -> Alt (comap f r1) (comap f r2)
--     App r1 r2     -> App (comap f r1) (comap f r2)
--     Fmap g r      -> Fmap g (comap f r)
--     Fail          -> Fail
--     Rep gr fn a r -> Rep gr fn a (comap f r)
--     Void r        -> Void (comap f r)

-- | Match and return a single symbol which satisfies the predicate
psym :: forall s. (s -> Boolean) -> RE s s
psym p = msym (\s -> if p s then Just s else Nothing)

-- | Like 'psym', but allows to return a computed value instead of the
-- original symbol
msym :: forall s a. (s -> Maybe a) -> RE s a
-- TODO: well this'll crash
msym p = symbol (defer \_ -> unsafeThrow "Not numbered symbol") p

-- | Match and return the given symbol
sym :: forall s. Eq s => s -> RE s s
sym s = psym ((==) s)

-- | Match and return any single symbol
anySym :: forall s. RE s s
anySym = msym Just

-- | Match and return the given sequence of symbols.
--
-- Note that there is an 'IsString' instance for regular expression, so
-- if you enable the @OverloadedStrings@ language extension, you can write
-- @string \"foo\"@ simply as @\"foo\"@.
--
-- Example:
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >import Text.Regex.Applicative
-- >
-- >number = "one" *> pure 1  <|>  "two" *> pure 2
-- >
-- >main = print $ "two" =~ number
string :: forall a. Eq a => Array a -> RE a (Array a)
string = traverse sym

-- | Match zero or more instances of the given expression, which are combined using
-- the given folding function.
--
-- 'Greediness' argument controls whether this regular expression should match
-- as many as possible ('Greedy') or as few as possible ('NonGreedy') instances
-- of the underlying expression.
reFoldl :: forall s a b. Greediness -> (b -> a -> b) -> b -> RE s a -> RE s b
reFoldl g f b a = rep g f b a

-- | Match zero or more instances of the given expression, but as
-- few of them as possible (i.e. /non-greedily/). A greedy equivalent of 'few'
-- is 'many'.
--
-- Examples:
--
-- >Text.Regex.Applicative> findFirstPrefix (few anySym  <* "b") "ababab"
-- >Just ("a","abab")
-- >Text.Regex.Applicative> findFirstPrefix (many anySym  <* "b") "ababab"
-- >Just ("ababa","")
few :: forall s a. RE s a -> RE s (Array a)
few a = reverse <$> rep NonGreedy (flip cons) [] a


-- helper
newtype R s a = R (RE s (Tuple a (Array s)))
derive instance newtypeR :: Newtype (R s a) _

-- | Return matched symbols as part of the return value
withMatched :: forall s a. RE s a -> RE s (Tuple a (Array s))
withMatched = go >>> unwrap where
  go :: RE s a -> R s a
  go = runFoldRE {
          eps: R $ flip Tuple [] <$> eps,
          symbol: \t p -> R $ symbol t (\s -> (flip Tuple [s]) <$> p s),
          alt: \a b -> R $ withMatched a <|> withMatched b,
          app: \a b -> R $ (\(Tuple f s) (Tuple x t) -> (Tuple (f x) (s <> t))) <$>
                  withMatched a <*>
                  withMatched b,
          fmap: \f x -> R $ (f *** id) <$> withMatched x,
          fail: R $ fail,
          rep: \gr f a0 x ->
            R $ rep gr
                (\(Tuple a s) (Tuple x t) -> (Tuple (f a x) (s <> t)))
                (Tuple a0 [])
                (withMatched x),
          -- N.B.: this ruins the Void optimization
          void: \x -> R $ (const unit *** id) <$> withMatched x
        }

-- | @s =~ a = match a s@
matchFlipped :: forall s a. (Array s) -> RE s a -> Maybe a
matchFlipped = flip match

infixl 2 matchFlipped as =~

-- | Attempt to match a string of symbols against the regular expression.
-- Note that the whole string (not just some part of it) should be matched.
--
-- Examples:
--
-- >Text.Regex.Applicative> match (sym 'a' <|> sym 'b') "a"
-- >Just 'a'
-- >Text.Regex.Applicative> match (sym 'a' <|> sym 'b') "ab"
-- >Nothing
--
match :: forall s a. RE s a -> Array s -> Maybe a
match re =
  let
    obj = compile re
  in
    \str ->
      head $
      results $
      foldl (flip step) obj str

-- | Find a string prefix which is matched by the regular expression.
--
-- Of all matching prefixes, pick one using left bias (prefer the left part of
-- '<|>' to the right part) and greediness.
--
-- This is the match which a backtracking engine (such as Perl's one) would find
-- first.
--
-- If match is found, the rest of the input is also returned.
--
-- Examples:
--
-- >Text.Regex.Applicative> findFirstPrefix ("a" <|> "ab") "abc"
-- >Just ("a","bc")
-- >Text.Regex.Applicative> findFirstPrefix ("ab" <|> "a") "abc"
-- >Just ("ab","c")
-- >Text.Regex.Applicative> findFirstPrefix "bc" "abc"
-- >Nothing
findFirstPrefix :: forall s a. RE s a -> Array s -> Maybe (Tuple a (Array s))
findFirstPrefix re str = go (compile re) str Nothing
  where
  walk obj lst = case uncons lst of
    Nothing -> Tuple obj Nothing
    Just { head: t, tail: ts } ->
    case getResult t of
      Just r -> Tuple obj $ Just r
      Nothing -> walk (addThread t obj) ts

  go obj str resOld =
    case walk emptyObject $ threads obj of
      (Tuple obj' resThis) ->
        let res = ((flip Tuple str) <$> resThis) <|> resOld
        in
          case uncons str of
            _ | failed obj' -> res
            Nothing -> res
            Just { head, tail } -> go (step head obj') tail res

-- | Find the longest string prefix which is matched by the regular expression.
--
-- Submatches are still determined using left bias and greediness, so this is
-- different from POSIX semantics.
--
-- If match is found, the rest of the input is also returned.
--
-- Examples:
--
-- >Text.Regex.Applicative Data.Char> let keyword = "if"
-- >Text.Regex.Applicative Data.Char> let identifier = many $ psym isAlpha
-- >Text.Regex.Applicative Data.Char> let lexeme = (Left <$> keyword) <|> (Right <$> identifier)
-- >Text.Regex.Applicative Data.Char> findLongestPrefix lexeme "if foo"
-- >Just (Left "if"," foo")
-- >Text.Regex.Applicative Data.Char> findLongestPrefix lexeme "iffoo"
-- >Just (Right "iffoo","")
findLongestPrefix :: forall s a. RE s a -> (Array s) -> Maybe (Tuple a (Array s))
findLongestPrefix re str = go (compile re) str Nothing
  where
  go obj str resOld =
    let res = (map (flip Tuple str) $ head $ results obj) <|> resOld
    in
      case uncons str of
        _ | failed obj -> res
        Nothing -> res
        Just { head, tail } -> go (step head obj) tail res

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix :: forall s a. RE s a -> (Array s) -> Maybe (Tuple a (Array s))
findShortestPrefix re str = go (compile re) str
  where
  go obj str =
    case uncons $ results obj of
      Just { head } -> Just (Tuple head str)
      _ | failed obj -> Nothing
      _ ->
        case uncons str of
          Nothing -> Nothing
          Just { head, tail } -> go (step head obj) tail

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findFirstInfix :: forall s a. RE s a -> (Array s) -> Maybe (Tuple (Array s) (Tuple a (Array s)))
findFirstInfix re str =
  map (\(Tuple (Tuple first res) last) -> Tuple first (Tuple res last)) $
  findFirstPrefix (Tuple <$> few anySym <*> re) str

-- Auxiliary function for findExtremeInfix
prefixCounter :: forall s. RE s (Tuple Int (Array s))
prefixCounter = second reverse <$> reFoldl NonGreedy f (Tuple 0 []) anySym
  where
  f (Tuple i prefix) s = (Tuple (i + 1)) $ cons s prefix

data InfixMatchingState s a = GotResult
  { prefixLen  :: Int
  , prefixStr  :: Array s
  , result   :: a
  , postfixStr :: Array s
  }
  | NoResult

-- a `preferOver` b chooses one of a and b, giving preference to a
preferOver :: forall s a.
  InfixMatchingState s a
  -> InfixMatchingState s a
  -> InfixMatchingState s a
preferOver NoResult b = b
preferOver b NoResult = b
preferOver a@(GotResult ar) b@(GotResult br) =
  case ar.prefixLen `compare` br.prefixLen of
    GT -> b -- prefer b when it has smaller prefix
    _  -> a -- otherwise, prefer a

mkInfixMatchingState :: forall s a.
  Array s -- rest of input
  -> Thread s (Tuple (Tuple Int (Array s)) a)
  -> InfixMatchingState s a
mkInfixMatchingState rest thread =
  case getResult thread of
    Just (Tuple (Tuple pLen pStr) res) ->
      GotResult
        { prefixLen:  pLen
        , prefixStr:  pStr
        , result:   res
        , postfixStr: rest
        }
    Nothing -> NoResult

gotResult :: forall s a. InfixMatchingState s a -> Boolean
gotResult (GotResult _) = true
gotResult _ = false

-- Algorithm for finding leftmost longest infix match:
--
-- 1. Add a thread /.*?/ to the begginning of the regexp
-- 2. As soon as we get first accept, we delete that thread
-- 3. When we get more than one accept, we choose one by the following criteria:
-- 3.1. Compare by the length of prefix (since we are looking for the leftmost
-- match)
-- 3.2. If they are produced on the same step, choose the first one (left-biased
-- choice)
-- 3.3. If they are produced on the different steps, choose the later one (since
-- they have the same prefixes, later means longer)
findExtremalInfix :: forall s a.
     -- function to combine a later result (first arg) to an earlier one (second
     -- arg)
     (InfixMatchingState s a -> InfixMatchingState s a -> InfixMatchingState s a)
  -> RE s a
  -> Array s
  -> Maybe (Tuple (Array s) (Tuple a (Array s)))
findExtremalInfix newOrOld re str =
  case go (compile $ Tuple <$> prefixCounter <*> re) str NoResult of
    NoResult -> Nothing
    GotResult r ->
      Just (Tuple (r.prefixStr) (Tuple (r.result) (r.postfixStr)))
  where
  {-
  go :: ReObject s ((Int, [s]), a)
     -> [s]
     -> InfixMatchingState s a
     -> InfixMatchingState s a
  -}
  go obj str resOld =
    let
      resThis = foldl
          (\acc t -> acc `preferOver` mkInfixMatchingState str t)
          NoResult $
          threads obj
      res = resThis `newOrOld` resOld
      obj' =
        -- If we just found the first result, kill the "prefixCounter" thread.
        -- We rely on the fact that it is the last thread of the object.
        if gotResult resThis && not (gotResult resOld)
          then fromMaybe obj $ fromThreads <$> (init $ threads obj)
          else obj
    in
      case uncons str of
        Nothing -> res
        _ | failed obj -> res
        Just { head, tail } -> go (step head obj') tail res


-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findLongestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findLongestInfix :: forall s a. RE s a -> Array s -> Maybe (Tuple (Array s) (Tuple a (Array s)))
findLongestInfix = findExtremalInfix preferOver

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findShortestInfix :: forall s a. RE s a -> Array s -> Maybe (Tuple (Array s) (Tuple a (Array s)))
findShortestInfix = findExtremalInfix $ flip preferOver

-- | Replace matches of the regular expression with its value.
--
-- >Text.Regex.Applicative > replace ("!" <$ sym 'f' <* some (sym 'o')) "quuxfoofooooofoobarfobar"
-- >"quux!!!bar!bar"
replace :: forall s. RE s (Array s) -> Array s -> Array s
replace r = ((#) []) <<< go
  where go ys = case findLongestInfix r ys of
                  Nothing -> (<>) ys
                  Just (Tuple before (Tuple m rest)) -> ((<>) before) <<< ((<>) m) <<< go rest
