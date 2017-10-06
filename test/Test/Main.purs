module Test.Main where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Gen (elements)
import Control.Plus (empty)
import Data.List.Lazy (List, concat, fromFoldable, nil, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Regex.Applicative (RE, few, many, findFirstPrefix, str, sym, withMatched, (=~))
import Data.String (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, Unit, discard, map, pure, show, unit, ($), (*>), (+), (<$>), (<*), (<*>), (<>))
import Test.QuickCheck (class Arbitrary, Result(Success), (==?))
import Test.Reference (reference)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)


-- Small alphabets
newtype A = A Char
derive instance newtypeA :: Newtype A _
derive newtype instance showA :: Show A
instance arbA :: Arbitrary A where
  arbitrary = elements $ A 'a' :| []
unA :: A -> Char
unA = unwrap

newtype AB = AB Char
derive instance newtypeAB :: Newtype AB _
derive newtype instance showAB :: Show AB
instance arbAB :: Arbitrary AB where
  arbitrary = elements $ AB 'a' :| [AB 'b']
unAB :: AB -> Char
unAB = unwrap

newtype ABC = ABC Char
derive instance newtypeABC :: Newtype ABC _
derive newtype instance showABC :: Show ABC
instance arbABC :: Arbitrary ABC where
  arbitrary = elements $ ABC 'a' :| [ABC 'b', ABC 'c']
unABC :: ABC -> Char
unABC = unwrap

-- Example Regexes

re1 :: RE Char (Tuple Int Int)
re1 =
  let
    one = pure 1 <* sym 'a'
    two = pure 2 <* sym 'a' <* sym 'a'
  in
    Tuple <$> (one <|> two) <*> (two <|> one)

re2 :: RE Char (List Int)
re2 = sequence $ fromFoldable $
  [ pure 1 <* sym 'a' <* sym 'a' <|>
    pure 2 <* sym 'a'
  , pure 3 <* sym 'b'
  , pure 4 <* sym 'b' <|>
    pure 5 <* sym 'a' ]

re3 :: RE Char (List Int)
re3 = sequence $ fromFoldable $
  [ pure 0 <|> pure 1
  , pure 1 <* sym 'a' <* sym 'a' <|>
    pure 2 <* sym 'a'
  , pure 3 <* sym 'b' <|> pure 6
  , map ((+) 1) $
    pure 4 <* sym 'b' <|>
    pure 7 <|>
    pure 5 <* sym 'a' ]

re4 :: RE Char (List Char)
re4 = sym 'a' *> many (sym 'b') <* sym 'a'

re5 :: RE Char (List Char)
re5 = (sym 'a' <|> sym 'a' *> sym 'a') *> many (sym 'a')

re6 :: RE Char (List Int)
re6 = many (pure 3 <* sym 'a' <* sym 'a' <* sym 'a' <|> pure 1 <* sym 'a')

data Quad a b c d = Quad a b c d
derive instance eqQuad :: (Eq a, Eq b, Eq c, Eq d) => Eq (Quad a b c d)
instance showQuad :: (Show a, Show b, Show c, Show d) => Show (Quad a b c d) where
  show (Quad a b c d) = "Quad " <> show a <> show b <> show c <> show d

-- Regular expression from the weighted regexp paper.
re7 :: RE Char (Tuple (List (Quad (List Char) Char (List Char) Char)) (List Char))
re7 =
  let
    many_A_or_B = many (sym 'a' <|> sym 'b')
  in
    Tuple <$>
        many (Quad <$> many_A_or_B <*> sym 'c' <*> many_A_or_B <*> sym 'c') <*>
        many_A_or_B

re8 :: RE Char (Tuple (List Char) (List Char))
re8 = Tuple <$> many (sym 'a' <|> sym 'b') <*> many (sym 'b' <|> sym 'c')

-- NB: we don't test these against the reference impl, 'cause it will loop!
re9 :: RE Char (List Char)
re9 = many (sym 'a' <|> empty) <* sym 'b'

re10 :: RE Char (List Char)
re10 = few (sym 'a' <|> empty) <* sym 'b'

prop :: forall c b a. Eq b => Show b =>
        RE a b ->
        (c -> a) ->
        Array c ->
        Result
prop re f s =
  let
    fs = map f (fromFoldable s)
  in
    reference re fs ==? (fs =~ re)

prop_withMatched :: Array AB -> Result
prop_withMatched =
  let
    re = withMatched $ many (str "a" <|> str "ba")
  in
    \s ->
      case map unAB s =~ re of
        Nothing -> Success
        Just (Tuple x y) -> concat x ==? y


-- Because we have 2 slightly different algorithms for recognition and parsing,
-- we test that they agree
testRecognitionAgainstParsing :: forall a b c. RE a b -> (c -> a) -> List c -> Result
testRecognitionAgainstParsing re f s =
  let
    fs = map f s
  in
    isJust (fs =~ re) ==? isJust (fs =~ (re *> pure unit))


main :: forall e. Eff (QCRunnerEffects e) Unit
main = run [consoleReporter] $ do
  describe "Tests" $ do
    -- describe "Engine tests" $ do
    --   it "re1" $ quickCheck $ prop re1 unA
    --   it "re2" $ quickCheck $ prop re2 unAB
    --   it "re3" $ quickCheck $ prop re3 unAB
    --   it "re4" $ quickCheck $ prop re4 unAB
    --   it "re5" $ quickCheck $ prop re5 unA
    --   it "re6" $ quickCheck $ prop re6 unA
    --   it "re7" $ quickCheck $ prop re7 unAB
    --   it "re8" $ quickCheck $ prop re8 unAB
    describe "Recognition vs parsing" $ do
    --   it "re1" $ quickCheck $ testRecognitionAgainstParsing re1 unA
    --   it "re2" $ quickCheck $ testRecognitionAgainstParsing re2 unAB
    --   it "re3" $ quickCheck $ testRecognitionAgainstParsing re3 unAB
    --   it "re4" $ quickCheck $ testRecognitionAgainstParsing re4 unAB
    --   it "re5" $ quickCheck $ testRecognitionAgainstParsing re5 unA
    --   it "re6" $ quickCheck $ testRecognitionAgainstParsing re6 unA
    --   it "re7" $ quickCheck $ testRecognitionAgainstParsing re7 unABC
    --   it "re8" $ quickCheck $ testRecognitionAgainstParsing re8 unABC
    --   it "re8" $ quickCheck $ testRecognitionAgainstParsing re9 unAB
    --   it "re8" $ quickCheck $ testRecognitionAgainstParsing re10 unAB
      it "withMatched" $ quickCheck prop_withMatched
    describe "Tests for matching functions" $ do
      describe "findFirstPrefix" $ do
        it "t1" $ quickCheck $
            (findFirstPrefix (str "a" <|> str "ab") (fromFoldable $ toCharArray "abc")) ==?
            (Just (Tuple ('a' : nil) ('b' : 'c' : nil)))
        it "t2" $ quickCheck $
            (findFirstPrefix (str "ab" <|> str "a") (fromFoldable $ toCharArray "abc")) ==?
            (Just (Tuple ('a' : 'b' : nil) ('c' : nil)))
      --   it "t3" $ quickCheck $
      --       (findFirstPrefix "bc" "abc") ==?
      --       Nothing
      -- describe "findFirstInfix" $ do
      --   it "t1" $ quickCheck $
      --       (findFirstInfix ("a" <|> "ab") "tabc") ==?
      --       (Just (Tuple "t" (Tuple "a" "bc")))
      --   it "t2" $ quickCheck $
      --       (findFirstInfix ("ab" <|> "a") "tabc") ==?
      --       (Just (Tuple "t" (Tuple "ab" "c")))
      -- describe "findLongestPrefix" $ do
      --   it "t1" $ quickCheck $
      --       (findLongestPrefix ("a" <|> "ab") "abc") ==?
      --       (Just (Tuple "ab" "c"))
      --   it "t2" $ quickCheck $
      --       (findLongestPrefix ("ab" <|> "a") "abc") ==?
      --       (Just (Tuple "ab" "c"))
      --   it "t3" $ quickCheck $
      --       (findLongestPrefix "bc" "abc") ==?
      --       Nothing
      -- describe "findLongestInfix" $ do
      --   it "t1" $ quickCheck $
      --       (findLongestInfix ("a" <|> "ab") "tabc") ==?
      --       (Just (Tuple "t" (Tuple "ab" "c")))
      --   it "t2" $ quickCheck $
      --       (findLongestInfix ("ab" <|> "a") "tabc") ==?
      --       (Just (Tuple "t" (Tuple "ab" "c")))
      --   it "t3" $ quickCheck $
      --       (findLongestInfix "bc" "tabc") ==?
      --       (Just (Tuple "ta" (Tuple "bc" "")))
      -- describe "findShortestPrefix" $ do
      --   it "t1" $ quickCheck $
      --       (findShortestPrefix ("a" <|> "ab") "abc") ==?
      --       (Just (Tuple "a" "bc"))
      --   it "t2" $ quickCheck $
      --       (findShortestPrefix ("ab" <|> "a") "abc") ==?
      --       (Just (Tuple "a" "bc"))
      --   it "t3" $ quickCheck $
      --       (findShortestPrefix "bc" "abc") ==?
      --       Nothing
      -- describe "findShortestInfix" $ do
      --   it "t1" $ quickCheck $
      --       (findShortestInfix ("a" <|> "ab") "tabc") ==?
      --       (Just (Tuple "t" (Tuple "a" "bc")))
      --   it "t2" $ quickCheck $
      --       (findShortestInfix ("ab" <|> "a") "tabc") ==?
      --       (Just (Tuple "t" (Tuple "a" "bc")))
      --   it "t3" $ quickCheck $
      --       (findShortestInfix "bc" "tabc") ==?
      --       (Just (Tuple "ta" (Tuple "bc" "")))
      -- describe "replace" $ do
      --   it "t1" $ quickCheck $
      --       (replace ("x" <$ "a" <|> "y" <$ "ab") "tabc") ==?
      --       "tyc"
      --   it "t2" $ quickCheck $
      --       (replace ("y" <$ "ab" <|> "x" <$ "a") "tabc") ==?
      --       "tyc"
      --   it "t3" $ quickCheck $
      --       (replace ("x" <$ "bc") "tabc") ==?
      --       "tax"
      --   it "t4" $ quickCheck $
      --       (replace ("y" <$ "a" <|> "x" <$ "ab") "tacabc") ==?
      --       "tycxc"
