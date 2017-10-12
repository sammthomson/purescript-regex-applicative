module Data.Regex.Applicative.Interface where

import Control.Alternative (empty, pure, (<|>))
import Control.Apply (lift2)
import Data.Array as A
import Data.List.Lazy (List, foldl, fromFoldable, head, init, nil, reverse, toUnfoldable, uncons, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong (second)
import Data.Regex.Applicative.Compile (Thread, emptyRe, addThread, compile, failed, fromThreads, getResult, results, step, threads)
import Data.Regex.Applicative.Types (Greediness(..), Re, ThreadId(..), elimRe, mkStar, mkSymbol)
import Data.String (fromCharArray, joinWith, singleton, toCharArray)
import Data.Traversable (class Foldable, class Traversable, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable)
import Prelude (class Eq, Ordering(GT), compare, flip, map, not, ($), (&&), (+), (<$>), (<*>), (<<<), (<>), (==))


star :: forall c a. Greediness -> Re c a -> Re c (Array a)
star g a = (A.fromFoldable <<< reverse) <$> mkStar g (flip (:)) nil a

-- | `(v)*`. Matches `v` 0 or more times.
many' :: forall c a. Re c a -> Re c (Array a)
many' = star Greedy

many :: Re Char String -> Re Char String
many re = joinWith "" <$> many' re

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
few' :: forall c a. Re c a -> Re c (Array a)
few' = star NonGreedy

few :: forall c. Re c String -> Re c String
few re = joinWith "" <$> few' re

-- | `(v)+`.  Matches `v` 1 or more times.
some' :: forall c a. Re c a -> Re c (Array a)
some' v = A.(:) <$> v <*> many' v

some :: forall c. Re c String -> Re c String
some re = joinWith "" <$> some' re

-- | Match and return a single symbol which satisfies the predicate
psym :: forall c. (c -> Boolean) -> Re c c
psym p = msym (\c -> if p c then Just c else Nothing)

-- | Like 'psym', but allows to return a computed value instead of the
-- | original symbol
-- 0 is a place-holder. will be renumbered during compilation
msym :: forall c a. (c -> Maybe a) -> Re c a
msym p = mkSymbol (ThreadId 0) p

-- | Match and return the given symbol
sym' :: forall c. Eq c => c -> Re c c
sym' c = psym ((==) c)

sym :: Char -> Re Char String
sym c = singleton <$> sym' c

-- | Match and return any single symbol
anySym' :: forall c. Re c c
anySym' = msym Just

anySym :: Re Char String
anySym = singleton <$> anySym'

-- | Match and return the given sequence of symbols.
seq :: forall a t. Eq a => Traversable t =>
       t a -> Re a (t a)
seq = traverse sym'

str :: String -> Re Char String
str s = fromCharArray <$> (seq $ toCharArray s)


-- | Return matched symbols as part of the return value
withMatched :: forall c a. Re c a -> Re c (Tuple (Array c) a)
withMatched = go where
  go = elimRe {
    eps: \a -> Tuple [] <$> pure a
    , fail: empty
    , symbol: \i p -> mkSymbol i (\c -> Tuple ([c]) <$> p c)
    , alt: \a b -> withMatched a <|> withMatched b
    , app: \a b -> lift2 (<*>) (withMatched a) (withMatched b)
    , map: \f x -> second f <$> withMatched x
    , star: \g op z x -> mkStar g (lift2 op) (Tuple [] z) (withMatched x)
  }

-- | @s =~ a = match a s@
matchFlipped' :: forall c a t. Foldable t => t c -> Re c a -> Maybe a
matchFlipped' = flip match'

infixl 2 matchFlipped' as =~~


matchFlipped :: forall a. String -> Re Char a -> Maybe a
matchFlipped = flip match

infixl 2 matchFlipped as =~

match' :: forall c a t. Foldable t => Re c a -> t c -> Maybe a
match' re = let obj = compile re in
    \s -> head $ results $ foldl step obj s

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
match :: forall r. Re Char r -> String -> Maybe r
match re s = match' re (toCharArray s)

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
findFirstPrefix' :: forall c a t. Foldable t =>
                   Re c a -> t c -> Maybe (Tuple a (Array c))
findFirstPrefix' re s = (map A.fromFoldable) <$> go (compile re) (fromFoldable s) Nothing
  where
  walk obj lst = case uncons lst of
    Nothing -> Tuple obj Nothing
    Just { head, tail } ->
      case getResult head of
        Just r -> Tuple obj $ Just r
        Nothing -> walk (addThread obj head) tail

  go obj s' resOld =
    case walk emptyRe $ threads obj of
      (Tuple obj' resThis) ->
        let
          res = ((flip Tuple s') <$> resThis) <|> resOld
        in
          case uncons s' of
            _ | failed obj' -> res
            Nothing -> res
            Just { head, tail } -> go (step obj' head) tail res

findFirstPrefix :: forall a. Re Char a -> String -> Maybe (Tuple a String)
findFirstPrefix re s = map fromCharArray <$> findFirstPrefix' re (toCharArray s)

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
findLongestPrefix' :: forall c a t. Foldable t =>
                     Re c a -> t c -> Maybe (Tuple a (Array c))
findLongestPrefix' re s = map A.fromFoldable <$> go (compile re) (fromFoldable s) Nothing
  where
  go obj s' resOld =
    let res = (map (flip Tuple s') $ head $ results obj) <|> resOld
    in
      case uncons s' of
        _ | failed obj -> res
        Nothing -> res
        Just { head, tail } -> go (step obj head) tail res

findLongestPrefix :: forall a. Re Char a -> String -> Maybe (Tuple a String)
findLongestPrefix re s = map fromCharArray <$> findLongestPrefix' re (toCharArray s)

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix' :: forall c a t. Foldable t =>
                      Re c a -> t c -> Maybe (Tuple a (List c))
findShortestPrefix' re s = go (compile re) (fromFoldable s)
  where
  go obj s' =
    case uncons $ results obj of
      Just { head } -> Just (Tuple head s')
      _ | failed obj -> Nothing
      _ ->
        case uncons s' of
          Nothing -> Nothing
          Just { head, tail } -> go (step obj head) tail

findShortestPrefix :: forall a. Re Char a -> String -> Maybe (Tuple a String)
findShortestPrefix re s = convert <$> (findShortestPrefix' re $ toCharArray s) where
  convert (Tuple a xs) = Tuple a $ fromCharArray $ A.fromFoldable xs

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findFirstInfix' :: forall c a t. Foldable t =>
                  Re c a -> t c -> Maybe (Tuple (Array c) (Tuple a (Array c)))
findFirstInfix' re s =
  map (\(Tuple (Tuple first res) last) -> Tuple first (Tuple res last)) $
  findFirstPrefix' (Tuple <$> few' anySym' <*> re) (A.fromFoldable s)


findFirstInfix :: forall a. Re Char a ->
                               String ->
                               Maybe (Tuple String (Tuple a String))
findFirstInfix re s =
  map (\(Tuple (Tuple first res) last) -> Tuple first (Tuple res last)) $
  findFirstPrefix (Tuple <$> few anySym <*> re) s

-- | Auxiliary function for findExtremeInfix
prefixCounter :: forall c. Re c (Tuple Int (List c))
prefixCounter = second reverse <$> reversedPrefix where
  reversedPrefix = mkStar NonGreedy op (Tuple 0 nil) anySym'
  op (Tuple i acc) c = Tuple (i + 1) $ c : acc

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
    -> Re c a
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
findLongestInfix' :: forall c a t. Foldable t =>
                    Re c a -> t c -> Maybe (Tuple (List c) (Tuple a (List c)))
findLongestInfix' r = findExtremalInfix preferOver r <<< fromFoldable

findLongestInfix :: forall a. Re Char a -> String -> Maybe (Tuple String (Tuple a String))
findLongestInfix r s =
  let
    list2str = fromCharArray <<< A.fromFoldable
    convert = \(Tuple pre (Tuple a post)) ->
                Tuple (list2str pre) (Tuple a (list2str post))
  in
    convert <$> (findLongestInfix' r $ toCharArray s)

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findShortestInfix' :: forall c a t. Foldable t =>
                     Re c a -> t c -> Maybe (Tuple (List c) (Tuple a (List c)))
findShortestInfix' r = findExtremalInfix (flip preferOver) r <<< fromFoldable

findShortestInfix :: forall a. Re Char a -> String -> Maybe (Tuple String (Tuple a String))
findShortestInfix r s =
  let
    list2str = fromCharArray <<< A.fromFoldable
    convert = \(Tuple pre (Tuple a post)) ->
                Tuple (list2str pre) (Tuple a (list2str post))
  in
    convert <$> (findShortestInfix' r $ toCharArray s)

-- | Replace matches of the regular expression with its value.
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Regex.Applicative
-- |    import Data.String (fromCharArray, toCharArray)
-- |
-- |    fromCharArray $ replace (['!'] <$ sym 'f' <* some (sym 'o')) $ toCharArray "quuxfoofooooofoobarfobar"
-- |    "quux!!!bar!bar"
-- | ```
replace :: forall t c. Foldable t => Unfoldable t =>
           Re c (t c) -> t c -> t c
replace r xs = toUnfoldable $ go (fromFoldable xs) nil where
  go ys = case findLongestInfix' r ys of
    Nothing -> \zs -> ys <> zs
    Just (Tuple before (Tuple m rest)) -> \zs ->
      before <> (fromFoldable m) <> (go rest zs)

replaceStr :: Re Char String -> String -> String
replaceStr r s = fromCharArray $ replace (toCharArray <$> r) $ toCharArray s
