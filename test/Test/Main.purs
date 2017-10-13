--------------------------------------------------------------------
-- |
-- | Module  : Test.Main
-- | Copyright : (c) Roman Cheplyaka, 2011; Sam Thomson, 2017.
-- | License   : MIT
-- |
-- | Tests for `Data.Regex.Applicative`.
--------------------------------------------------------------------
module Test.Main where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Gen (elements)
import Control.Plus (empty)
import Data.List.Lazy (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Regex.Applicative (Re, few, findFirstInfix, findFirstPrefix, findLongestInfix, findLongestPrefix, findShortestInfix, findShortestPrefix, many, str, sym, withMatched, (=~))
import Data.Regex.Applicative.Interface (many', match', replace, sym', (=~~))
import Data.String (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple4, tuple4)
import Prelude (class Eq, class Show, Unit, discard, map, pure, ($), (*>), (+), (<$>), (<$), (<*), (<*>), (<>))
import Test.QuickCheck (class Arbitrary, Result(..), (==?))
import Test.Reference (reference)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.StateQueue (stateQueueTests)
import Test.Url (urlTest)
-- import Test.Expression (expressionTests)


-- Small alphabets
newtype A = A Char
derive instance newtypeA :: Newtype A _
derive newtype instance showA :: Show A
instance arbA :: Arbitrary A where
  arbitrary = elements $ A 'a' :| []

newtype AB = AB Char
derive instance newtypeAB :: Newtype AB _
derive newtype instance showAB :: Show AB
instance arbAB :: Arbitrary AB where
  arbitrary = elements $ AB <$> 'a' :| ['b']

newtype ABC = ABC Char
derive instance newtypeABC :: Newtype ABC _
derive newtype instance showABC :: Show ABC
instance arbABC :: Arbitrary ABC where
  arbitrary = elements $ ABC <$> 'a' :| ['b', 'c']

-- Example Regexes
re0 :: Re Char String
re0 = (pure "uh" <* sym 'a') <> pure "mazing"

re1 :: Re Char (Tuple Int Int)
re1 = Tuple <$> (one <|> two) <*> (two <|> one) where
  one = pure 1 <* sym 'a'
  two = pure 2 <* sym 'a' <* sym 'a'

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

re4 :: Re Char String
re4 = sym 'a' *> many (sym 'b') <* sym 'a'

re5 :: Re Char String
re5 = (sym 'a' <|> sym 'a' *> sym 'a') *> many (sym 'a')

re6 :: Re Char (Array Int)
re6 = many' (pure 3 <* sym 'a' <* sym 'a' <* sym 'a' <|> pure 1 <* sym 'a')

-- Regular expression from the weighted regexp paper.
re7 :: Re Char (Tuple (Array (Tuple4 String Char String Char)) String)
re7 =
  let manyAOrB = many (sym 'a' <|> sym 'b')
  in Tuple <$> many' (tuple4 <$> manyAOrB
                             <*> sym' 'c'
                             <*> manyAOrB
                             <*> sym' 'c')
           <*> manyAOrB

re8 :: Re Char (Tuple String String)
re8 = Tuple <$> many (sym 'a' <|> sym 'b') <*> many (sym 'b' <|> sym 'c')

re9 :: Re Char String
re9 = many (sym 'a' <|> empty) <* sym 'b'

re10 :: Re Char String
re10 = few (sym 'a' <|> empty) <* sym 'b'

testMatchVsRef :: forall c b a. Eq b => Show b => Newtype c a =>
              (a -> c) ->  -- constructor (ignored, just to specify Newtype)
              Re a b ->
              Array c ->
              Result
testMatchVsRef _ re s = realResult ==? refResult where
  s' = map unwrap $ fromFoldable s
  refResult = reference re s'
  realResult = match' re s'

testWithMatched :: Re Char String -> Array AB -> Result
testWithMatched re s =
  case (fromCharArray $ map unwrap s) =~ withMatched re of
    Nothing -> Success
    Just (Tuple x y) -> x ==? y


main :: forall e. Eff (QCRunnerEffects e) Unit
main = run [consoleReporter] $ do
  let check = quickCheck' 1
  describe "Tests" $ do
    describe "Url test" $ do
      urlTest
    describe "StateQueue" do
      stateQueueTests
    -- FIXME: infinite loop:
    -- describe "Expression" $ do
    --   expressionTests
    describe "Properties" $ do
      describe "Matching vs reference" $ do
        it "re0"  $ quickCheck $ testMatchVsRef A re0
        it "re1"  $ quickCheck $ testMatchVsRef A re1
        it "re2"  $ quickCheck $ testMatchVsRef AB re2
        it "re3"  $ quickCheck $ testMatchVsRef AB re3
        it "re4"  $ quickCheck $ testMatchVsRef AB re4
        it "re5"  $ quickCheck $ testMatchVsRef A re5
        it "re6"  $ quickCheck $ testMatchVsRef A re6
        it "re7"  $ quickCheck $ testMatchVsRef AB re7
        it "re8"  $ quickCheck $ testMatchVsRef AB re8
        it "re9"  $ quickCheck $ testMatchVsRef AB re9
        it "re10" $ quickCheck $ testMatchVsRef AB re10
      describe "withMatched" $ do
        it "withMatched" $ quickCheck $ testWithMatched (many (str "a" <|> str "ba"))
    describe "Fixtures" $ do
      describe "Matching vs reference" $ do
        let a1 = ['a']
        it "re0" $ check $ (a1 =~~ re0) ==? reference re0 a1
        it "re9" $ check $ (a1 =~~ re9) ==? reference re9 a1
      describe "findFirstPrefix" $ do
        it "t1" $ check $
          findFirstPrefix (str "a" <|> str "ab") "abc" ==?
            Just (Tuple "a" "bc")
        it "t2" $ check $
          findFirstPrefix (str "ab" <|> str "a") "abc" ==?
            Just (Tuple "ab" "c")
        it "t3" $ check $
          findFirstPrefix (str "bc") "abc" ==?
            Nothing
      describe "findFirstInfix" $ do
        it "t1" $ check $
          (findFirstInfix (str "a" <|> str "ab") "tabc") ==?
            (Just (Tuple "t" (Tuple "a" "bc")))
        it "t2" $ check $
          (findFirstInfix (str "ab" <|> str "a") "tabc") ==?
            (Just (Tuple "t" (Tuple "ab" "c")))
      describe "findLongestPrefix" $ do
        it "t1" $ check $
          (findLongestPrefix (str "a" <|> str "ab") "abc") ==?
            (Just (Tuple "ab" "c"))
        it "re9" $ check $
          (findLongestPrefix re9 "abc") ==?
            (Just (Tuple "a" "c"))
        it "t2" $ check $
          (findLongestPrefix (str "ab" <|> str "a") "abc") ==?
            (Just (Tuple "ab" "c"))
        it "t3" $ check $
          (findLongestPrefix (str "bc") "abc") ==?
            Nothing
      describe "findLongestInfix" $ do
        it "t1" $ check $
          (findLongestInfix (str "a" <|> str "ab") "tabc") ==?
            (Just (Tuple "t" (Tuple "ab" "c")))
        it "t2" $ check $
          (findLongestInfix (str "ab" <|> str "a") "tabc") ==?
            (Just (Tuple "t" (Tuple "ab" "c")))
        it "t3" $ check $
          (findLongestInfix (str "bc") "tabc") ==?
            (Just (Tuple "ta" (Tuple "bc" "")))
      describe "findShortestPrefix" $ do
        it "t1" $ check $
          (findShortestPrefix (str "a" <|> str "ab") "abc") ==?
            (Just (Tuple "a" "bc"))
        it "t2" $ check $
          (findShortestPrefix (str "ab" <|> str "a") "abc") ==?
            (Just (Tuple "a" "bc"))
        it "t3" $ check $
          (findShortestPrefix (str "bc") "abc") ==?
            Nothing
      describe "findShortestInfix" $ do
        it "t1" $ check $
          (findShortestInfix (str "a" <|> str "ab") "tabc") ==?
            (Just (Tuple "t" (Tuple "a" "bc")))
        it "t2" $ check $
          (findShortestInfix (str "ab" <|> str "a") "tabc") ==?
            (Just (Tuple "t" (Tuple "a" "bc")))
        it "t3" $ check $
          (findShortestInfix (str "bc") "tabc") ==?
            (Just (Tuple "ta" (Tuple "bc" "")))
      describe "replace" $ do
        it "t1" $ check $
          (replace ("x" <$ str "a" <|> "y" <$ str "ab") "tabc") ==?
            "tyc"
        it "t2" $ check $
          (replace ("y" <$ str "ab" <|>  "x" <$ str "a") "tabc") ==?
            "tyc"
        it "t3" $ check $
          (replace ("x" <$ str "bc") "tabc") ==?
            "tax"
        it "t4" $ check $
          (replace ("y" <$ str "a" <|> "x" <$ str "ab") "tacabc") ==?
            "tycxc"
