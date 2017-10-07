module Data.Regex.Applicative.Interface where

import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.List.Lazy (List, cons, foldl, fromFoldable, head, init, nil, reverse, uncons, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong (second, (***))
import Data.Regex.Applicative.Object (addThread, compile, emptyObject, failed, fromThreads, getResult, results, step, threads)
import Data.Regex.Applicative.Types (Greediness(..), RE, Thread, mkEps, mkFail, mkRep, mkSymbol, runFoldRE)
import Data.String (toCharArray)
import Data.Traversable (class Foldable, class Traversable, traverse)
import Data.Tuple (Tuple(..), swap)
import Prelude (class Eq, class Semigroup, Ordering(GT), compare, const, flip, id, map, not, unit, (#), ($), (&&), (+), (<$>), (<*>), (<<<), (<>), (==))


-- | `(v)*`. Matches `v` 0 or more times.
many :: forall c a. RE c a -> RE c (List a)
many v = reverse <$> mkRep Greedy (flip (:)) nil v

-- | `(v)+`.  Matches `v` 0 or more times.
some :: forall c a. RE c a -> RE c (List a)
some v = (:) <$> v <*> defer (\_ -> many v)

-- | Match and return a single symbol which satisfies the predicate
psym :: forall c. (c -> Boolean) -> RE c c
psym p = msym (\c -> if p c then Just c else Nothing)

-- | Like 'psym', but allows to return a computed value instead of the
-- original symbol
msym :: forall c a. (c -> Maybe a) -> RE c a
msym p = mkSymbol (defer \_ -> unsafeThrow "Not numbered symbol") p

-- | Match and return the given symbol
sym :: forall c. Eq c => c -> RE c c
sym c = psym ((==) c)

-- | Match and return any single symbol
anySym :: forall c. RE c c
anySym = msym Just

-- | Match and return the given sequence of symbols.
--
-- Note that there is an 'IsString' instance for regular expression, so
-- if you enable the @OverloadedStrings@ language extension, you can write
-- @str \"foo\"@ simply as @\"foo\"@.
--
-- Example:
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >import Text.Regex.Applicative
-- >
-- >number = "one" *> pure 1  <|>  "two" *> pure 2
-- >
-- >main = print $ "two" =~ number
arr :: forall a t. Eq a => Traversable t => t a -> RE a (t a)
arr = traverse sym

str :: String -> RE Char (List Char)
str s = fromFoldable <$> (arr $ toCharArray s)

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
few :: forall c a. RE c a -> RE c (List a)
few a = reverse <$> mkRep NonGreedy (flip cons) nil a


-- helper
newtype R c a = R (RE c (Tuple a (List c)))

-- | Return matched symbols as part of the return value
withMatched :: forall c a. RE c a -> RE c (Tuple a (List c))
withMatched r = case go r of R r' -> r' where
  applyTuple :: forall x y z. Semigroup z => Tuple (x -> y) z -> Tuple x z -> Tuple y z
  applyTuple f x = swap $ swap f <*> swap x
  go :: RE c a -> R c a
  go = runFoldRE {
    eps: R $ flip Tuple nil <$> mkEps,
    symbol: \t p -> R $ mkSymbol t (\s -> (flip Tuple (s : nil)) <$> p s),
    alt: \a b -> R $ withMatched a <|> withMatched b,
    app: \a b -> R $ applyTuple <$> withMatched a
                                <*> withMatched b,
    fmap: \f x -> R $ (f *** id) <$> withMatched x,
    fail: R $ mkFail,
    rep: \gr f a0 x ->
      R $ mkRep gr
          (\(Tuple a s) (Tuple x' t) -> (Tuple (f a x') (s <> t)))
          (Tuple a0 nil)
          (withMatched x),
    -- N.B.: this ruins the Void optimization
    void: \x -> R $ (const unit *** id) <$> withMatched x
  }

-- | @s =~ a = match a s@
matchFlipped :: forall c a t. Foldable t => t c -> RE c a -> Maybe a
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
match :: forall c a t. Foldable t => RE c a -> t c -> Maybe a
match re =
  let
    obj = compile re
  in
    \s -> head $ results $ foldl (flip step) obj s

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
findFirstPrefix :: forall c a. RE c a -> List c -> Maybe (Tuple a (List c))
findFirstPrefix re s = go (compile re) s Nothing
  where
  walk obj lst = case uncons lst of
    Nothing -> Tuple obj Nothing
    Just { head, tail } ->
      case getResult head of
        Just r -> Tuple obj $ Just r
        Nothing -> walk (addThread head obj) tail

  go obj s' resOld =
    case walk emptyObject $ threads obj of
      (Tuple obj' resThis) ->
        let
          res = ((flip Tuple s') <$> resThis) <|> resOld
        in
          case uncons s' of
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
findLongestPrefix :: forall c a. RE c a -> List c -> Maybe (Tuple a (List c))
findLongestPrefix re s = go (compile re) s Nothing
  where
  go obj s' resOld =
    let res = (map (flip Tuple s') $ head $ results obj) <|> resOld
    in
      case uncons s' of
        _ | failed obj -> res
        Nothing -> res
        Just { head, tail } -> go (step head obj) tail res

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix :: forall c a. RE c a -> List c -> Maybe (Tuple a (List c))
findShortestPrefix re s = go (compile re) s
  where
  go obj s' =
    case uncons $ results obj of
      Just { head } -> Just (Tuple head s')
      _ | failed obj -> Nothing
      _ ->
        case uncons s' of
          Nothing -> Nothing
          Just { head, tail } -> go (step head obj) tail

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findFirstInfix :: forall c a. RE c a -> List c -> Maybe (Tuple (List c) (Tuple a (List c)))
findFirstInfix re s =
  map (\(Tuple (Tuple first res) last) -> Tuple first (Tuple res last)) $
  findFirstPrefix (Tuple <$> few anySym <*> re) s

-- Auxiliary function for findExtremeInfix
prefixCounter :: forall c. RE c (Tuple Int (List c))
prefixCounter = second reverse <$> mkRep NonGreedy f (Tuple 0 nil) anySym
  where
  f (Tuple i prefix) s = (Tuple (i + 1)) $ cons s prefix

data InfixMatchingState c a = GotResult
  { prefixLen  :: Int
  , prefixStr  :: List c
  , result     :: a
  , postfixStr :: List c
  }
  | NoResult

-- a `preferOver` b chooses one of a and b, giving preference to a
preferOver :: forall c a.
  InfixMatchingState c a
  -> InfixMatchingState c a
  -> InfixMatchingState c a
preferOver NoResult b = b
preferOver b NoResult = b
preferOver a@(GotResult ar) b@(GotResult br) =
  case ar.prefixLen `compare` br.prefixLen of
    GT -> b -- prefer b when it has smaller prefix
    _  -> a -- otherwise, prefer a

mkInfixMatchingState :: forall c a.
  List c -- rest of input
  -> Thread c (Tuple (Tuple Int (List c)) a)
  -> InfixMatchingState c a
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

gotResult :: forall c a. InfixMatchingState c a -> Boolean
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
findExtremalInfix :: forall c a.
     -- function to combine a later result (first arg) to an earlier one (second
     -- arg)
     (InfixMatchingState c a -> InfixMatchingState c a -> InfixMatchingState c a)
  -> RE c a
  -> List c
  -> Maybe (Tuple (List c) (Tuple a (List c)))
findExtremalInfix newOrOld re s =
  case go (compile $ Tuple <$> prefixCounter <*> re) s NoResult of
    NoResult -> Nothing
    GotResult r ->
      Just (Tuple (r.prefixStr) (Tuple (r.result) (r.postfixStr)))
  where
    go obj s' resOld =
      let
        resThis = foldl
            (\acc t -> acc `preferOver` mkInfixMatchingState s' t)
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
        case uncons s' of
          Nothing -> res
          _ | failed obj -> res
          Just { head, tail } -> go (step head obj') tail res


-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findLongestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findLongestInfix :: forall c a. RE c a -> List c -> Maybe (Tuple (List c) (Tuple a (List c)))
findLongestInfix = findExtremalInfix preferOver

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findShortestInfix :: forall c a. RE c a -> List c -> Maybe (Tuple (List c) (Tuple a (List c)))
findShortestInfix = findExtremalInfix $ flip preferOver

-- | Replace matches of the regular expression with its value.
--
-- >Text.Regex.Applicative > replace ("!" <$ sym 'f' <* some (sym 'o')) "quuxfoofooooofoobarfobar"
-- >"quux!!!bar!bar"
replace :: forall c. RE c (List c) -> List c -> List c
replace r = ((#) nil) <<< go
  where go ys = case findLongestInfix r ys of
                  Nothing -> (<>) ys
                  Just (Tuple before (Tuple m rest)) -> ((<>) before) <<< ((<>) m) <<< go rest
