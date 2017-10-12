module Test.Main where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Gen (elements)
import Control.Plus (empty)
import Data.List.Lazy (List, fromFoldable, nil, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty ((:|))
import Data.Regex.Applicative (Re, few, findFirstInfix, findFirstPrefix, findLongestPrefix, many, str, sym, withMatched, (=~))
import Data.String (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, Unit, discard, join, map, pure, show, unit, ($), (*>), (+), (<$>), (<*), (<*>), (<>))
import Test.QuickCheck (class Arbitrary, Result(..), (==?))
import Test.Reference (reference)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)


-- Small alphabets
newtype A = A Char
derive newtype instance showA :: Show A
instance arbA :: Arbitrary A where
  arbitrary = elements $ A 'a' :| []
unA :: A -> Char
unA (A c) = c

newtype AB = AB Char
derive newtype instance showAB :: Show AB
instance arbAB :: Arbitrary AB where
  arbitrary = elements $ AB 'a' :| [AB 'b']
unAB :: AB -> Char
unAB (AB c) = c

newtype ABC = ABC Char
derive newtype instance showABC :: Show ABC
instance arbABC :: Arbitrary ABC where
  arbitrary = elements $ ABC 'a' :| [ABC 'b', ABC 'c']
unABC :: ABC -> Char
unABC (ABC c) = c

-- Example Regexes

re0 :: Re Char String
re0 = (pure "uh" <* sym 'a') <> pure "mazing"

re1 :: Re Char (Tuple Int Int)
re1 =
  let
    one = pure 1 <* sym 'a'
    two = pure 2 <* sym 'a' <* sym 'a'
  in
    Tuple <$> (one <|> two) <*> (two <|> one)

re2 :: Re Char (Array Int)
re2 = sequence
  [ pure 1 <* sym 'a' <* sym 'a' <|>
      pure 2 <* sym 'a'
  , pure 3 <* sym 'b'
  , pure 4 <* sym 'b' <|>
      pure 5 <* sym 'a' ]

re3 :: Re Char (List Int)
re3 = sequence $ fromFoldable $
  [ pure 0 <|> pure 1
  , pure 1 <* sym 'a' <* sym 'a' <|>
      pure 2 <* sym 'a'
  , pure 3 <* sym 'b' <|> pure 6
  , map ((+) 1) $
      pure 4 <* sym 'b' <|>
      pure 7 <|>
      pure 5 <* sym 'a' ]

re4 :: Re Char (List Char)
re4 = sym 'a' *> many (sym 'b') <* sym 'a'

re5 :: Re Char (List Char)
re5 = (sym 'a' <|> sym 'a' *> sym 'a') *> many (sym 'a')

re6 :: Re Char (List Int)
re6 = many (pure 3 <* sym 'a' <* sym 'a' <* sym 'a' <|> pure 1 <* sym 'a')

data Quad a b c d = Quad a b c d
derive instance eqQuad :: (Eq a, Eq b, Eq c, Eq d) => Eq (Quad a b c d)
instance showQuad :: (Show a, Show b, Show c, Show d) => Show (Quad a b c d) where
  show (Quad a b c d) = "Quad " <> show a <> show b <> show c <> show d

-- Regular expression from the weighted regexp paper.
re7 :: Re Char (Tuple (List (Quad (List Char) Char (List Char) Char)) (List Char))
re7 =
  let
    many_A_or_B = many (sym 'a' <|> sym 'b')
  in
    Tuple <$>
        many (Quad <$> many_A_or_B <*> sym 'c' <*> many_A_or_B <*> sym 'c') <*>
        many_A_or_B

re8 :: Re Char (Tuple (List Char) (List Char))
re8 = Tuple <$> many (sym 'a' <|> sym 'b') <*> many (sym 'b' <|> sym 'c')

re9 :: Re Char (List Char)
re9 = many (sym 'a' <|> empty) <* sym 'b'

re10 :: Re Char (List Char)
re10 = few (sym 'a' <|> empty) <* sym 'b'

prop :: forall b a. Eq b => Show a => Show b =>
           Re a b ->
           Array a ->
           Result
prop re s = realResult ==? refResult where
  fs = fromFoldable s
  refResult = reference re fs
  realResult = fs =~ re


propMap :: forall c b a. Eq b => Show a => Show b =>
           Re a b ->
           (c -> a) ->
           Array c ->
           Result
propMap re f s = prop re $ map f s


prop_withMatched :: Array AB -> Result
prop_withMatched =
  let
    re = withMatched $ many (str "a" <|> str "ba")
  in
    \s -> case map unAB s =~ re of
      Nothing -> Success
      Just (Tuple x y) -> x ==? join y


-- Because we have 2 slightly different algorithms for recognition and parsing,
-- we test that they agree
testRecognitionAgainstParsing :: forall a b c. Re a b -> (c -> a) -> Array c -> Result
testRecognitionAgainstParsing re f s =
  let
    fs = map f s
  in
    isJust (fs =~ re) ==? isJust (fs =~ (re *> pure unit))


main :: forall e. Eff (QCRunnerEffects e) Unit
main = run [consoleReporter] $ do
  describe "Tests" $ do
    describe "Fixtures" $ do
      let a1 = 'a' : nil
      it "re0" $ quickCheck' 1 $ (a1 =~ re0) ==? reference re0 a1
      it "re9" $ quickCheck' 1 $ (a1 =~ re9) ==? reference re9 a1
    describe "Matching vs reference" $ do
      it "re0" $ quickCheck $ propMap re0 unA
      it "re1" $ quickCheck $ propMap re1 unA
      it "re2" $ quickCheck $ propMap re2 unAB
      it "re3" $ quickCheck $ propMap re3 unAB
      it "re4" $ quickCheck $ propMap re4 unAB
      it "re5" $ quickCheck $ propMap re5 unA
      it "re6" $ quickCheck $ propMap re6 unA
      it "re7" $ quickCheck $ propMap re7 unAB
      it "re8" $ quickCheck $ propMap re8 unAB
      it "re9" $ quickCheck $ propMap re9 unAB
      it "re10" $ quickCheck $ propMap re10 unAB
    describe "Recognition vs parsing" $ do
      it "re1" $ quickCheck $ testRecognitionAgainstParsing re1 unA
      it "re2" $ quickCheck $ testRecognitionAgainstParsing re2 unAB
      it "re3" $ quickCheck $ testRecognitionAgainstParsing re3 unAB
      it "re4" $ quickCheck $ testRecognitionAgainstParsing re4 unAB
      it "re5" $ quickCheck $ testRecognitionAgainstParsing re5 unA
      it "re6" $ quickCheck $ testRecognitionAgainstParsing re6 unA
      it "re7" $ quickCheck $ testRecognitionAgainstParsing re7 unABC
      it "re8" $ quickCheck $ testRecognitionAgainstParsing re8 unABC
      it "re9" $ quickCheck $ testRecognitionAgainstParsing re9 unAB
      it "re10" $ quickCheck $ testRecognitionAgainstParsing re10 unAB
      it "withMatched" $ quickCheck prop_withMatched
    describe "Matching functions" $ do
      describe "findFirstPrefix" $ do
        it "t1" $ quickCheck' 1 $
          findFirstPrefix (str "a" <|> str "ab") (toCharArray "abc") ==?
          Just (Tuple ('a' : nil) ('b' : 'c' : nil))
        it "t2" $ quickCheck' 1 $
          findFirstPrefix (str "ab" <|> str "a") (toCharArray "abc") ==?
          Just (Tuple ('a' : 'b' : nil) ('c' : nil))
        it "t3" $ quickCheck' 1 $
          findFirstPrefix (str "bc") (toCharArray "abc") ==?
          Nothing
      describe "findFirstInfix" $ do
        it "t1" $ quickCheck' 1 $
          (findFirstInfix (str "a" <|> str "ab") (toCharArray "tabc")) ==?
          (Just (Tuple ('t' : nil) (Tuple ('a' : nil) ('b' : 'c' : nil))))
        it "t2" $ quickCheck' 1 $
          (findFirstInfix (str "ab" <|> str "a") (toCharArray "tabc")) ==?
          (Just (Tuple ('t' : nil) (Tuple ('a' : 'b' : nil) ('c' : nil))))
      describe "findLongestPrefix" $ do
        it "t1" $ quickCheck' 1 $
          (findLongestPrefix (str "a" <|> str "ab") (toCharArray "abc")) ==?
          (Just (Tuple ('a' : 'b' : nil) ('c' : nil)))
        it "re9" $ quickCheck' 1 $
          (findLongestPrefix re9 (toCharArray "abc")) ==?
          (Just (Tuple ('a' : nil) ('c' : nil)))
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
