module Data.Regex.Applicative.Interface (
  -- functions on Strings
  (=~)
  , sym
  , psym
  , msym
  , anySym
  , str
  , few
  , findFirstPrefix
  , findLongestPrefix
  , findShortestPrefix
  , findFirstInfix
  , findLongestInfix
  , findShortestInfix
  , many
  , match
  , matchFlipped
  , replace
  , some
  , withMatched
  -- functions on sequences
  , (=~~)
  , sym'
  , psym'
  , anySym'
  , seq
  , few'
  , findFirstPrefix'
  , findLongestPrefix'
  , findShortestPrefix'
  , findFirstInfix'
  , findLongestInfix'
  , findShortestInfix'
  , many'
  , match'
  , matchFlipped'
  , replace'
  , some'
  , withMatched'
) where

import Control.Alternative (empty, pure, (<|>))
import Control.Apply (lift2)
import Data.Array as A
import Data.List.Lazy (List, foldl, fromFoldable, head, init, nil, reverse, toUnfoldable, uncons, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong (first, second)
import Data.Regex.Applicative.Compile (Thread, emptyRe, addThread, compile, failed, fromThreads, getResult, results, step, threads)
import Data.Regex.Applicative.Types (Greediness(..), Re, ThreadId(..), elimRe, mkStar, mkSymbol)
import Data.String (fromCharArray, joinWith, singleton, toCharArray)
import Data.Traversable (class Foldable, class Traversable, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable)
import Prelude (class Eq, Ordering(GT), compare, flip, map, not, ($), (&&), (+), (<$>), (<*>), (<<<), (<>), (==))


star :: forall c a. Greediness -> Re c a -> Re c (Array a)
star g a = (A.fromFoldable <<< reverse) <$> mkStar g (flip (:)) nil a

-- | Generalized version of `many` that works on `Foldable`s of symbols.
many' :: forall c a. Re c a -> Re c (Array a)
many' = star Greedy

-- | Match zero or more instances of the given expression, but as
-- | many of them as possible (i.e. /greedily/).
-- | A non-greedy equivalent of 'few' is 'many'.
-- |
-- | Example:
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Regex.Applicative
-- |
-- |    findFirstPrefix (many anySym <* str "b") "ababab"
-- |    -- (Just (Tuple "ababa" ""))
-- | ```
many :: Re Char String -> Re Char String
many re = joinWith "" <$> many' re

-- | Generalized version of `few` that works on `Foldable`s of symbols.
few' :: forall c a. Re c a -> Re c (Array a)
few' = star NonGreedy

-- | Match zero or more instances of the given expression, but as
-- | few of them as possible (i.e. /non-greedily/). A greedy equivalent of 'few'
-- | is 'many'.
-- |
-- | Example:
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Regex.Applicative
-- |
-- |    findFirstPrefix (few anySym <* str "b") "ababab"
-- |    -- (Just (Tuple "a" "abab"))
-- | ```
few :: forall c. Re c String -> Re c String
few re = joinWith "" <$> few' re

-- | Generalized version of `some` that works on `Foldable`s of symbols.
some' :: forall c a. Re c a -> Re c (Array a)
some' v = A.(:) <$> v <*> many' v

-- | `(v)+`.  Matches `v` 1 or more times.
some :: forall c. Re c String -> Re c String
some re = joinWith "" <$> some' re

-- | Generalized version of `psym` that works on any type of symbols.
psym' :: forall c. (c -> Boolean) -> Re c c
psym' p = msym (\c -> if p c then Just c else Nothing)

-- | Match and return a single symbol which satisfies the predicate
psym :: (Char -> Boolean) -> Re Char String
psym p = singleton <$> psym' p

-- | Like 'psym', but allows to return a computed value instead of the
-- | original symbol
-- 0 is a place-holder. will be renumbered during compilation
msym :: forall c a. (c -> Maybe a) -> Re c a
msym p = mkSymbol (ThreadId 0) p

-- | Generalized version of `sym` that works on `Foldable`s of symbols.
sym' :: forall c. Eq c => c -> Re c c
sym' c = psym' ((==) c)

-- | Match and return the given symbol
sym :: Char -> Re Char String
sym c = singleton <$> sym' c

-- | Generalized version of `anySym` that works on `Foldable`s of symbols.
anySym' :: forall c. Re c c
anySym' = msym Just

-- | Match and return any single symbol
anySym :: Re Char String
anySym = singleton <$> anySym'

-- | Match and return the given sequence of symbols.
seq :: forall a t. Eq a => Traversable t =>
       t a -> Re a (t a)
seq = traverse sym'

-- | Match and return the given String.
str :: String -> Re Char String
str s = fromCharArray <$> (seq $ toCharArray s)


-- | Generalized version of `withMatched` that works on `Foldable`s of symbols.
withMatched' :: forall c a. Re c a -> Re c (Tuple (Array c) a)
withMatched' = go where
  go :: forall c' r. Re c' r -> Re c' (Tuple (Array c') r)
  go = elimRe
    { eps: \a -> Tuple [] <$> pure a
    , fail: empty
    , symbol: \i p -> mkSymbol i (\c -> Tuple [c] <$> p c)
    , alt: \a b -> go a <|> go b
    , app: \a b -> lift2 (<*>) (go a) (go b)
    , star: \g op z x -> mkStar g (lift2 op) (Tuple [] z) (go x)
    , map: \f x -> second f <$> go x
    }

-- | Return matched string as part of the return value
withMatched :: forall a. Re Char a -> Re Char (Tuple String a)
withMatched re = first fromCharArray <$> withMatched' re

-- | `s =~~ a = match' a s`
matchFlipped' :: forall c a t. Foldable t => t c -> Re c a -> Maybe a
matchFlipped' = flip match'

infixl 2 matchFlipped' as =~~


-- | `s =~ a = match a s`
matchFlipped :: forall a. String -> Re Char a -> Maybe a
matchFlipped = flip match

infixl 2 matchFlipped as =~

-- | Generalized version of `match` that works on `Foldable`s of symbols.
match' :: forall c a t. Foldable t => Re c a -> t c -> Maybe a
match' re = let obj = compile re in
    \s -> head $ results $ foldl step obj s

-- | Attempt to match a string against the regular expression.
-- | Note that the whole string (not just some part of it) should be matched.
-- |
-- | Examples:
-- |
-- | ```
-- |    import Prelude
-- |    import Control.Alt
-- |    import Data.Regex.Applicative
-- |
-- |    match (sym 'a' <|> sym 'b') "a"
-- |    -- Just "a"
-- |    match (sym 'a' <|> sym 'b') "ab"
-- |    -- Nothing
-- | ```
match :: forall r. Re Char r -> String -> Maybe r
match re s = match' re (toCharArray s)

-- | Generalized version of `findFirstPrefix` that works on `Foldable`s.
findFirstPrefix' :: forall c a t. Foldable t =>
                    Re c a -> t c -> Maybe (Tuple a (List c))
findFirstPrefix' re s = go (compile re) (fromFoldable s) Nothing where
  -- `r` is the current state.
  -- `s'` is remaining string left to match.
  -- `oldResult` is the current best match.
  go r s' oldResult =
    case walk emptyRe $ threads r of
      Tuple r' newResult ->
        let  -- prefer newer results (they come from higher priority threads)
          res = (flip Tuple s' <$> newResult) <|> oldResult
        in case uncons s' of
          -- continue over rest of string, using only higher priority threads
          Just { head, tail } | not (failed r') -> go (step r' head) tail res
          _ -> res

  -- `walk` searches through the list of `Threads` `ts` for an `Accept r`.
  -- If one is found then `r` is returned, along with all higher priority
  -- threads (which could still potentially match a longer prefix).
  -- Lower priority threads can be safely discarded at that point.
  walk higherPriority ts =
    case uncons ts of
      Nothing -> Tuple higherPriority Nothing
      Just { head, tail } ->
        case getResult head of
          Just r -> Tuple higherPriority (Just r)
          Nothing -> walk (addThread head higherPriority) tail

-- | Find a string prefix which is matched by the regular expression.
-- |
-- | Of all matching prefixes, pick one using left bias (prefer the left part of
-- | '<|>' to the right part), breaking ties by Greediness (longer/shorter).
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
-- |
-- |    findFirstPrefix (str "a" <|> str "ab") "abc"
-- |    -- (Just (Tuple "a" "bc"))
-- |    findFirstPrefix (str "ab" <|> str "a") "abc"
-- |    -- (Just (Tuple "ab" "c"))
-- |    findFirstPrefix (str "bc") "abc"
-- |    -- Nothing
-- | ```
findFirstPrefix :: forall a. Re Char a -> String -> Maybe (Tuple a String)
findFirstPrefix re s =
  map (fromCharArray <<< A.fromFoldable) <$> findFirstPrefix' re (toCharArray s)

-- | Generalized version of `findLongestPrefix` that works on `Foldable`s of symbols.
findLongestPrefix' :: forall c a t. Foldable t =>
                      Re c a ->
                      t c ->
                      Maybe (Tuple a (Array c))
findLongestPrefix' re s =
  map A.fromFoldable <$> go (compile re) (fromFoldable s) Nothing where
    go r s' oldRes =
      let
        newRes = flip Tuple s' <$> head (results r)
        res = newRes <|> oldRes  -- prefer new result
      in
        case uncons s' of
          Just { head, tail } | not (failed r) -> go (step r head) tail res
          _  -> res

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
-- |    import Data.Char.Unicode
-- |    import Data.Either
-- |    import Data.Regex.Applicative
-- |
-- |    keyword = str "if"
-- |    identifier = many $ psym isAlpha
-- |    lexeme = (Left <$> keyword) <|> (Right <$> identifier)
-- |    findLongestPrefix lexeme "if foo"
-- |    -- (Just (Tuple (Left "if") " foo"))
-- |    findLongestPrefix lexeme "iffoo"
-- |    -- (Just (Tuple (Right "iffoo") ""))
-- | ```
findLongestPrefix :: forall a. Re Char a -> String -> Maybe (Tuple a String)
findLongestPrefix re s = map fromCharArray <$> findLongestPrefix' re (toCharArray s)

-- | Generalized version of `findShortestPrefix` that works on `Foldable`s of symbols.
findShortestPrefix' :: forall c a t. Foldable t =>
                      Re c a ->
                      t c ->
                      Maybe (Tuple a (List c))
findShortestPrefix' re s = go (compile re) (fromFoldable s) where
  go r s' =
    case uncons (results r) of
      Just { head } -> Just (Tuple head s')
      _ | failed r -> Nothing
      _ ->
        case uncons s' of
          Nothing -> Nothing
          Just { head, tail } -> go (step r head) tail

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix :: forall a. Re Char a -> String -> Maybe (Tuple a String)
findShortestPrefix re s = convert <$> (findShortestPrefix' re $ toCharArray s) where
  convert = map (fromCharArray <<< A.fromFoldable)

-- | Generalized version of `findFirstInfix` that works on `Foldable`s of symbols.
findFirstInfix' :: forall c a t. Foldable t =>
                  Re c a ->
                  t c ->
                  Maybe (Tuple (Array c) (Tuple a (List c)))
findFirstInfix' re s =
  map (\(Tuple (Tuple first res) last) -> Tuple first (Tuple res last)) $
  findFirstPrefix' (Tuple <$> few' anySym' <*> re) s

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
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
    go obj s' old =
      let
        resThis = foldl
            (\acc t -> acc `preferOver` mkInfixMatchingState s' t)
            NoResult $
            threads obj
        res = resThis `newOrOld` old
        obj' =
          -- If we just found the first result, kill the "prefixCounter" thread.
          -- We rely on the fact that it is the last thread of the object.
          if gotResult resThis && not (gotResult old)
            then fromMaybe obj $ fromThreads <$> (init $ threads obj)
            else obj
      in
        case uncons s' of
          Nothing -> res
          _ | failed obj -> res
          Just { head, tail } -> go (step obj' head) tail res


-- | Generalized version of `findLongestInfix` that works on `Foldable`s of symbols.
findLongestInfix' :: forall c a t. Foldable t =>
                    Re c a -> t c -> Maybe (Tuple (List c) (Tuple a (List c)))
findLongestInfix' r = findExtremalInfix preferOver r <<< fromFoldable

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findLongestPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findLongestInfix :: forall a. Re Char a -> String -> Maybe (Tuple String (Tuple a String))
findLongestInfix r s =
  let
    list2str = fromCharArray <<< A.fromFoldable
    convert = \(Tuple pre (Tuple a post)) ->
                Tuple (list2str pre) (Tuple a (list2str post))
  in
    convert <$> (findLongestInfix' r $ toCharArray s)

-- | Generalized version of `findShortestInfix` that works on `Foldable`s of symbols.
findShortestInfix' :: forall c a t. Foldable t =>
                     Re c a -> t c -> Maybe (Tuple (List c) (Tuple a (List c)))
findShortestInfix' r = findExtremalInfix (flip preferOver) r <<< fromFoldable

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findShortestInfix :: forall a. Re Char a -> String -> Maybe (Tuple String (Tuple a String))
findShortestInfix r s =
  let
    list2str = fromCharArray <<< A.fromFoldable
    convert = \(Tuple pre (Tuple a post)) ->
                Tuple (list2str pre) (Tuple a (list2str post))
  in
    convert <$> (findShortestInfix' r $ toCharArray s)

-- | Generalized version of `replace` that works on `Foldable`s of symbols.
replace' :: forall t c. Foldable t => Unfoldable t =>
           Re c (t c) -> t c -> t c
replace' r xs = toUnfoldable $ go (fromFoldable xs) nil where
  go ys = case findLongestInfix' r ys of
    Nothing -> \zs -> ys <> zs
    Just (Tuple before (Tuple m rest)) -> \zs ->
      before <> (fromFoldable m) <> (go rest zs)

-- | Replace matches of the regular expression with its value.
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Regex.Applicative
-- |
-- |    replace ("!" <$ sym 'f' <* some (sym 'o')) "quuxfoofooooofoobarfobar"
-- |    -- "quux!!!bar!bar"
-- | ```
replace :: Re Char String -> String -> String
replace r s = fromCharArray $ replace' (toCharArray <$> r) $ toCharArray s
