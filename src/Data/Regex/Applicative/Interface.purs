module Data.Regex.Applicative.Interface where

import Control.Alternative ((<|>))
import Control.Apply (lift2)
import Data.List.Lazy (List, cons, foldl, fromFoldable, head, init, nil, reverse, uncons, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong (second)
import Data.Regex.Applicative.Object (addThread, compile, emptyObject, failed, fromThreads, getResult, results, step, threads)
import Data.Regex.Applicative.Types (Greediness(NonGreedy, Greedy), RE(..), Thread, ThreadId(ThreadId), elimRE, mkStar)
import Data.String (toCharArray)
import Data.Traversable (class Foldable, class Traversable, traverse)
import Data.Tuple (Tuple(Tuple))
import Prelude (class Eq, Ordering(GT), compare, flip, map, not, (#), ($), (&&), (+), (<$>), (<*>), (<<<), (<>), (==))


-- | `(v)*`. Matches `v` 0 or more times.
many :: forall c a. RE c a -> RE c (List a)
many v = reverse <$> mkStar Greedy (flip (:)) nil v

-- | `(v)+`.  Matches `v` 0 or more times.
some :: forall c a. RE c a -> RE c (List a)
some v = (:) <$> v <*> many v

-- | Match and return a single symbol which satisfies the predicate
psym :: forall c. (c -> Boolean) -> RE c c
psym p = msym (\c -> if p c then Just c else Nothing)

-- | Like 'psym', but allows to return a computed value instead of the
-- | original symbol
-- 0 is a place-holder. will be renumbered during compilation
msym :: forall c a. (c -> Maybe a) -> RE c a
msym p = Symbol (ThreadId 0) p

-- | Match and return the given symbol
sym :: forall c. Eq c => c -> RE c c
sym c = psym ((==) c)

-- | Match and return any single symbol
anySym :: forall c. RE c c
anySym = msym Just

-- | Match and return the given sequence of symbols.
arr :: forall a t. Eq a => Traversable t => t a -> RE a (t a)
arr = traverse sym

str :: String -> RE Char (List Char)
str s = fromFoldable <$> (arr $ toCharArray s)

-- | Match zero or more instances of the given expression, but as
-- | few of them as possible (i.e. /non-greedily/). A greedy equivalent of 'few'
-- | is 'many'.
-- |
-- | Examples:
-- |
-- | ```
-- |     findFirstPrefix (few anySym  <* "b") "ababab"
-- |     -- Just ("a","abab")
-- |     findFirstPrefix (many anySym  <* "b") "ababab"
-- |     -- Just ("ababa","")
-- | ```
few :: forall c a. RE c a -> RE c (List a)
few a = reverse <$> mkStar NonGreedy (flip cons) nil a


-- | Return matched symbols as part of the return value
withMatched :: forall c a. RE c a -> RE c (Tuple (List c) a)
withMatched = go where
  go = elimRE {
    eps: \a -> Tuple nil <$> Eps a
    , fail: Fail
    , symbol: \i p -> Symbol i (\c -> Tuple (c : nil) <$> p c)
    , alt: \a b -> withMatched a <|> withMatched b
    , app: \a b -> lift2 (<*>) (withMatched a) (withMatched b)
    , fmap: \f x -> second f <$> withMatched x
    , star: \g op z x -> mkStar g (lift2 op) (Tuple nil z) (withMatched x)
  }

-- | @s =~ a = match a s@
matchFlipped :: forall c a t. Foldable t => t c -> RE c a -> Maybe a
matchFlipped = flip match

infixl 2 matchFlipped as =~

-- | Attempt to match a string of symbols against the regular expression.
-- | Note that the whole string (not just some part of it) should be matched.
-- |
-- | Examples:
-- |
-- | ```
-- |    import Prelude
-- |    import Control.Alt
-- |    import Data.String (toCharArray)
-- |
-- |    match (sym 'a' <|> sym 'b') $ toCharArray "a"
-- |    -- Just 'a'
-- |    match (sym 'a' <|> sym 'b') $ toCharArray "ab"
-- |    -- Nothing
-- | ```
match :: forall c a t. Foldable t => RE c a -> t c -> Maybe a
match re = let obj = compile re in
    \s -> head $ results $ foldl step obj s

-- | Find a string prefix which is matched by the regular expression.
-- |
-- | Of all matching prefixes, pick one using left bias (prefer the left part of
-- | '<|>' to the right part) and greediness.
-- |
-- | This is the match which a backtracking engine (such as Perl's one) would find
-- | first.
-- |
-- | If match is found, the rest of the input is also returned.
-- |
-- | Examples:
-- |
-- | ```
-- |    import Prelude
-- |    import Control.Alt
-- |    import Data.Regex.Applicative
-- |    import Data.String (toCharArray)
-- |
-- |    findFirstPrefix (str "a" <|> str "ab") $ toCharArray "abc"
-- |    -- Just ("a", "bc")
-- |    findFirstPrefix (str "ab" <|> str "a") $ toCharArray "abc"
-- |    -- Just ("ab", "c")
-- |    findFirstPrefix "bc" "abc"
-- |    -- Nothing
-- | ```
findFirstPrefix :: forall c a t. Foldable t =>
                   RE c a -> t c -> Maybe (Tuple a (List c))
findFirstPrefix re s = go (compile re) (fromFoldable s) Nothing
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
            Just { head, tail } -> go (step obj' head) tail res

-- | Find the longest string prefix which is matched by the regular expression.
-- |
-- | Submatches are still determined using left bias and greediness, so this is
-- | different from POSIX semantics.
-- |
-- | If match is found, the rest of the input is also returned.
-- |
-- | Examples:
-- | ```
-- |    import Prelude
-- |    import Control.Alt
-- |    import Data.Regex.Applicative
-- |    import Data.Char.Unicode
-- |    import Data.Either
-- |    import Data.String
-- |
-- |    keyword = str "if"
-- |    identifier = many $ psym isAlpha
-- |    lexeme = (Left <$> keyword) <|> (Right <$> identifier)
-- |    findLongestPrefix lexeme $ toCharArray "if foo"
-- |    -- Just (Tuple (Left ('i' : 'f' : nil)) (' ' : 'f' : 'o' : 'o' : nil))
-- |    findLongestPrefix lexeme $ toCharArray "iffoo"
-- |    -- Just (Tuple (Right ('i' : 'f' : 'f' : 'o' : 'o' : nil)) nil)
-- | ```
findLongestPrefix :: forall c a t. Foldable t =>
                     RE c a -> t c -> Maybe (Tuple a (List c))
findLongestPrefix re s = go (compile re) (fromFoldable s) Nothing
  where
  go obj s' resOld =
    let res = (map (flip Tuple s') $ head $ results obj) <|> resOld
    in
      case uncons s' of
        _ | failed obj -> res
        Nothing -> res
        Just { head, tail } -> go (step obj head) tail res

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix :: forall c a t. Foldable t =>
                      RE c a -> t c -> Maybe (Tuple a (List c))
findShortestPrefix re s = go (compile re) (fromFoldable s)
  where
  go obj s' =
    case uncons $ results obj of
      Just { head } -> Just (Tuple head s')
      _ | failed obj -> Nothing
      _ ->
        case uncons s' of
          Nothing -> Nothing
          Just { head, tail } -> go (step obj head) tail

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findFirstInfix :: forall c a t. Foldable t =>
                  RE c a -> t c -> Maybe (Tuple (List c) (Tuple a (List c)))
findFirstInfix re s =
  map (\(Tuple (Tuple first res) last) -> Tuple first (Tuple res last)) $
  findFirstPrefix (Tuple <$> few anySym <*> re) (fromFoldable s)

-- | Auxiliary function for findExtremeInfix
prefixCounter :: forall c. RE c (Tuple Int (List c))
prefixCounter = second reverse <$> mkStar NonGreedy f (Tuple 0 nil) anySym
  where
  f (Tuple i prefix) s = (Tuple (i + 1)) $ cons s prefix

data InfixMatchingState c a = GotResult
  { prefixLen  :: Int
  , prefixStr  :: List c
  , result     :: a
  , postfixStr :: List c
  }
  | NoResult

-- | a `preferOver` b chooses one of a and b, giving preference to a
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

-- | Algorithm for finding leftmost longest infix match:
-- |
-- | 1. Add a thread /.*?/ to the begginning of the regexp
-- | 2. As soon as we get first accept, we delete that thread
-- | 3. When we get more than one accept, we choose one by the following criteria:
-- | 3.1. Compare by the length of prefix (since we are looking for the leftmost
-- | match)
-- | 3.2. If they are produced on the same step, choose the first one (left-biased
-- | choice)
-- | 3.3. If they are produced on the different steps, choose the later one (since
-- | they have the same prefixes, later means longer)
findExtremalInfix :: forall c a t. Foldable t =>
     -- function to combine a later result (first arg) to an earlier one (second
     -- arg)
     (InfixMatchingState c a -> InfixMatchingState c a -> InfixMatchingState c a)
  -> RE c a
  -> t c
  -> Maybe (Tuple (List c) (Tuple a (List c)))
findExtremalInfix newOrOld re s =
  case go (compile $ Tuple <$> prefixCounter <*> re) (fromFoldable s) NoResult of
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
          Just { head, tail } -> go (step obj' head) tail res


-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findLongestPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findLongestInfix :: forall c a t. Foldable t =>
                    RE c a -> t c -> Maybe (Tuple (List c) (Tuple a (List c)))
findLongestInfix r = findExtremalInfix preferOver r <<< fromFoldable

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findShortestInfix :: forall c a t. Foldable t =>
                     RE c a -> t c -> Maybe (Tuple (List c) (Tuple a (List c)))
findShortestInfix r = findExtremalInfix (flip preferOver) r <<< fromFoldable

-- | Replace matches of the regular expression with its value.
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Array as A
-- |    import Data.Regex.Applicative
-- |    import Data.List.Lazy hiding (some)
-- |    import Data.String (fromCharArray, toCharArray)
-- |
-- |    fromCharArray $ A.fromFoldable $ replace (('!' : nil) <$ sym 'f' <* some (sym 'o')) $ fromFoldable $ toCharArray "quuxfoofooooofoobarfobar"
-- |    "quux!!!bar!bar"
-- | ```
replace :: forall c. RE c (List c) -> List c -> List c
replace r = ((#) nil) <<< go
  where go ys = case findLongestInfix r ys of
                  Nothing -> (<>) ys
                  Just (Tuple before (Tuple m rest)) -> ((<>) before) <<< ((<>) m) <<< go rest
