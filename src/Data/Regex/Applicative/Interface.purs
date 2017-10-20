--------------------------------------------------------------------
-- |
-- | Module    : Data.Regex.Applicative.Interface
-- | Copyright : (c) Sam Thomson, 2017
-- | Copyright : (c) Roman Cheplyaka, 2011
-- | License   : MIT
-- |
-- | User-facing API for constructing and matching regexes.
--------------------------------------------------------------------
module Data.Regex.Applicative.Interface (
  InfixMatch(..)
  , (=~)
  , anySingleton
  , anySym
  , few
  , foldFew
  , foldMany
  , foldSome
  , findFirstPrefix
  , findLongestPrefix
  , findShortestPrefix
  , findFirstInfix
  , findLongestInfix
  , findShortestInfix
  , many
  , match
  , matchFlipped
  , msym
  , pSingleton
  , psym
  , replace
  , singleton
  , some
  , str
  , sym
  , withMatched
) where

import Prelude

import Control.Alternative (empty, (<|>))
import Control.Apply (lift2)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (List(..), foldl, uncons)
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (first)
import Data.Regex.Applicative.Compile (addThread, compile, emptyRe, failed, fromThreads, getResult, results, step, threads)
import Data.Regex.Applicative.Types (Greediness(..), Re, ThreadId(..), elimRe, mkStar, mkSymbol)
import Data.Regex.Applicative.SeqLike (class SeqLike, Seq(..), fromList, fromSeq, length, toList, toSeq)
import Data.Regex.Applicative.SeqLike as Seq
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


-- | The result of matching a regex on a string of type `s`.
-- | `before` is the prefix of the string before the match.
-- | `result` is the value returned by the regex.
-- | `after` is the postfix of the string after the match.
newtype InfixMatch s r =
  Match
    { before  :: s
    , result :: r
    , after :: s
    }

mkMatch :: forall s r. s -> r -> s -> InfixMatch s r
mkMatch before result after = Match { before, result, after }

-- helper function for mapping over `before` and `after`.
mapStr :: forall s r t. (s -> t) -> InfixMatch s r -> InfixMatch t r
mapStr f (Match a) = Match (a { before = f a.before, after = f a.after })


star :: forall c r. Greediness -> Re c r -> Re c (List r)
star g a = L.reverse <$> mkStar g (flip L.(:)) L.Nil a

-- | Match zero or more instances of the given expression, but as
-- | many of them as possible (i.e. *greedily*).
-- | A non-greedy equivalent of `few` is `many`.
-- |
-- | Example:
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Regex.Applicative
-- |
-- |    findFirstPrefix (many anySingleton <* str "b") "ababab"
-- |    -- (Just (Tuple "ababa" ""))
-- | ```
many :: forall c r. Re c r -> Re c (List r)
many = star Greedy

foldMany :: forall c s. Monoid s => Re c s -> Re c s
foldMany re = fold <$> many re

-- | Match zero or more instances of the given expression, but as
-- | few of them as possible (i.e. *non-greedily*).
-- | A greedy equivalent of `few` is `many`.
-- |
-- | Example:
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Regex.Applicative
-- |
-- |    findFirstPrefix (foldFew anySingleton <* str "b") "ababab"
-- |    -- (Just (Tuple "a" "abab"))
-- | ```
few :: forall c r. Re c r -> Re c (List r)
few = star NonGreedy

foldFew :: forall c r. Monoid r => Re c r -> Re c r
foldFew re = fold <$> few re

-- | `(v)+`.  Matches `v` 1 or more times.
some :: forall c r. Re c r -> Re c (List r)
some v = L.(:) <$> v <*> many v

foldSome :: forall c s. Monoid s => Re c s -> Re c s
foldSome re = fold <$> some re

-- | Match and return a single symbol which satisfies the predicate
psym :: forall c. (c -> Boolean) -> Re c c
psym p = msym (\c -> if p c then Just c else Nothing)

pSingleton :: forall c s. SeqLike s c => (c -> Boolean) -> Re c s
pSingleton p = Seq.singleton <$> psym p

-- | Like 'psym, but allows to return a computed value instead of the
-- | original symbol.
-- 0 is a place-holder. will be renumbered during compilation
msym :: forall c r. (c -> Maybe r) -> Re c r
msym p = mkSymbol (ThreadId 0) p

-- | Match and return the given symbol.
sym :: forall c. Eq c => c -> Re c c
sym c = psym ((==) c)

-- | Match and return the given symbol as a string.
singleton :: forall c s. SeqLike s c => Eq c => c -> Re c s
singleton c = Seq.singleton <$> sym c

-- | Match and return any single symbol.
anySym :: forall c. Re c c
anySym = msym Just

-- | Match and return any single symbol as a string.
anySingleton :: forall c s. SeqLike s c => Re c s
anySingleton = Seq.singleton <$> anySym

-- | Match and return the given String.
str :: forall c s. SeqLike s c => Eq c => s -> Re c s
str s = fromList <$> traverse sym (toList s)


-- | Return matched string as part of the return value
withMatched :: forall c s a. SeqLike s c => Re c a -> Re c (Tuple s a)
withMatched r = first unwrap <$> go r where
  go :: forall b. Re c b -> Re c (Tuple (Seq s) b)
  go = elimRe
    { eps: \a -> pure $ pure a
    , fail: empty
    , symbol: \i p -> mkSymbol i (\c -> Tuple (Seq (Seq.singleton c)) <$> p c)
    , alt: \a b -> go a <|> go b
    , app: \a b -> (<*>) <$> go a <*> go b
    , star: \g op z x -> mkStar g (lift2 op) (pure z) (go x)
    , map: \f x -> map f <$> go x
    }

-- | `s =~ a = match a s`
matchFlipped :: forall a. String -> Re Char a -> Maybe a
matchFlipped = flip match

infixl 2 matchFlipped as =~

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
match :: forall c s r. SeqLike s c => Re c r -> s -> Maybe r
match re = let obj = compile re in
    \s -> LL.head $ results $ foldl step obj $ toList s

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
findFirstPrefix :: forall c s r. SeqLike s c =>
                   Re c r -> s -> Maybe (InfixMatch s r)
findFirstPrefix re s = mapStr fromList <$> go (compile re) (toList s) Nothing where
  -- `r` is the current state.
  -- `s'` is remaining string left to match.
  -- `oldResult` is the current best match.
  go r s' oldResult =
    case walk emptyRe $ threads r of
      Tuple r' newResult ->
        let  -- prefer newer results (they come from higher priority threads)
          res = (flip (mkMatch empty) s' <$> newResult) <|> oldResult
        in case uncons s' of
          -- continue over rest of string, using only higher priority threads
          Just { head, tail } | not (failed r') -> go (step r' head) tail res
          _ -> res

  -- `walk` searches through the list of `Threads` `ts` for an `Accept r`.
  -- If one is found then `r` is returned, along with all higher priority
  -- threads (which could still potentially match a longer prefix).
  -- Lower priority threads can be safely discarded at that point.
  walk higherPriority ts =
    case LL.uncons ts of
      Nothing -> Tuple higherPriority Nothing
      Just { head, tail } ->
        case getResult head of
          Just r -> Tuple higherPriority (Just r)
          Nothing -> walk (addThread head higherPriority) tail

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
-- |    identifier = many $ pSingleton isAlpha
-- |    lexeme = (Left <$> keyword) <|> (Right <$> identifier)
-- |    findLongestPrefix lexeme "if foo"
-- |    -- (Just (Tuple (Left "if") " foo"))
-- |    findLongestPrefix lexeme "iffoo"
-- |    -- (Just (Tuple (Right "iffoo") ""))
-- | ```
findLongestPrefix :: forall c s r. SeqLike s c =>
                     Re c r ->
                     s ->
                     Maybe (InfixMatch s r)
findLongestPrefix re s =
  mapStr fromList <$> go (compile re) (toList s) Nothing where
    go r s' oldRes =
      let
        newRes = flip (mkMatch Nil) s' <$> LL.head (results r)
        res = newRes <|> oldRes  -- prefer new result
      in
        case uncons s' of
          Just { head, tail } | not (failed r) -> go (step r head) tail res
          _  -> res

-- | Find the shortest prefix (analogous to `findLongestPrefix`)
findShortestPrefix :: forall c s r. SeqLike s c =>
                      Re c r ->
                      s ->
                      Maybe (InfixMatch s r)
findShortestPrefix re s = mapStr fromList <$> go (compile re) (toList s) where
  go r s' =
    if failed r then Nothing else
      case LL.uncons (results r) of
        Just { head } -> Just $ Match { before: Nil, after: s', result: head }
        _ ->
          case uncons s' of
            Nothing -> Nothing
            Just { head, tail } -> go (step r head) tail

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like `findFirstPrefix`. Returns the result together with
-- | the prefix and suffix of the string surrounding the match.
findFirstInfix :: forall c s r. SeqLike s c =>
                  Re c r ->
                  s ->
                  Maybe (InfixMatch s r)
findFirstInfix re s =
  (\(Match { result, after }) -> result after) <$>
    findFirstPrefix (mkMatch <$> (unwrap <$> foldFew (Seq <$> anySingleton)) <*> re) s


-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like `findLongestPrefix`. Returns the result together
-- | with the prefix and suffix of the string surrounding the match.
findLongestInfix :: forall c s r. SeqLike s c =>
                    Re c r -> s -> Maybe (InfixMatch s r)
findLongestInfix = findExtremalInfix preferEarlier

-- | Find the leftmost substring that is matched by the regular expression.
-- | Otherwise behaves like `findShortestPrefix`. Returns the result together
-- | with the prefix and suffix of the string surrounding the match.
findShortestInfix :: forall c s r. SeqLike s c =>
                     Re c r -> s -> Maybe (InfixMatch s r)
findShortestInfix = findExtremalInfix $ flip preferEarlier

-- | Replace all matches of the regular expression with its value.
-- |
-- | ```
-- |    import Prelude
-- |    import Data.Regex.Applicative
-- |
-- |    replace ("!" <$ sym 'f' <* foldSome (sym 'o')) "quuxfoofooooofoobarfobar"
-- |    -- "quux!!!bar!bar"
-- | ```
replace :: forall c s r. SeqLike s c =>
           Re c s -> s -> s
replace r xs = fromSeq $ go (toSeq xs) [] where
  go ys = case findLongestInfix r ys of
    Nothing -> \zs -> ys <> zs
    Just (Match { before, result, after }) -> \zs ->
      before <> toSeq result <> go after zs

-- helper type for `findExtremalInfix`
type M c r = Maybe (InfixMatch (List c) r)

-- | Helper function for `findLongestInfix` and `findShortestInfix`.
-- | `a \`preferEarlier\` b` chooses one of a and b, giving preference to
-- | earlier matches, and breaking ties in favor of `a`.
preferEarlier :: forall c r. M c r -> M c r -> M c r
preferEarlier = maxBy (map (negate <<< prefixLen)) where
  maxBy f a b = if ((>=) `on` f) a b then a else b
  prefixLen (Match { before }) = length before


-- | Helper function for finding leftmost shortest/longest infix matches.
-- |
-- | Adds a `.*` thread to the begginning of the regexp.
-- | As soon as we get first accept, we delete the `.*` thread.
-- |
-- | If we get more than one accept, we choose the one with the shortest
-- | prefix (`before`). We break ties in favor of higher priority (left-bias).
-- | Our second tie-breaker is on length of match.
findExtremalInfix :: forall c s a n.
                     SeqLike s c =>
                     (M c a -> M c a -> M c a)  -- prefer earlier or later
                     -> Re c a
                     -> s
                     -> Maybe (InfixMatch s a)
findExtremalInfix op re s =
  mapStr fromList <$> go (compile prefixRe) (toList s) Nothing where
    -- non-greedy `.*`, then `re`. (`mkMatch` is still waiting for `after`)
    prefixRe = mkMatch <$> few anySym <*> re
    go obj s' oldRes =
      let
        -- `op` determines which result is kept here
        newRes = foldl (\acc r -> acc `op` (Just $ r s')) Nothing (results obj)
        res = newRes `op` oldRes
        obj' =
          -- If we just found the first result, kill the "prefixRe" thread.
          -- We rely on the fact that it is the last thread of the object.
          if isJust newRes && not (isJust oldRes)
            then fromMaybe obj $ fromThreads <$> (LL.init $ threads obj)
            else obj
      in
        case uncons s' of
          Just { head, tail } | not (failed obj) ->
              go (step obj' head) tail res
          _ -> res


derive instance eqInfixMatch :: (Eq s, Eq r) => Eq (InfixMatch s r)
derive instance functorInfixMatch :: Functor (InfixMatch s)
derive instance newtypeInfixMatch :: Newtype (InfixMatch s r) _
instance showInfixMatch :: (Show s, Show r) => Show (InfixMatch s r) where
  show (Match m) = "(Match { before: " <> show m.before <>
                          ", result: " <> show m.result <>
                          ", after: " <> show m.after <> "})"
