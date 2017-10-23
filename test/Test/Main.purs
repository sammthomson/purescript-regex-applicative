--------------------------------------------------------------------
-- |
-- | Module  : Test.Main
-- | Copyright : (c) Roman Cheplyaka, 2011; Sam Thomson, 2017.
-- | License   : MIT
-- |
-- | Tests for `Data.Regex.Applicative`.
--------------------------------------------------------------------
module Test.Main where

import Prelude

import Data.Regex.Applicative
  ( InfixMatch(..)
  , Re
  , findFirstInfix
  , findFirstPrefix
  , findLongestInfix
  , findLongestPrefix
  , findShortestInfix
  , findShortestPrefix
  , foldFew
  , foldMany
  , many
  , match
  , replace
  , singleton
  , str
  , sym
  , withMatched
  , (=~)
  )
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Gen (elements)
import Control.Plus (empty)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.String (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple4, tuple4)
import Test.Expression (expressionTest)
import Test.QuickCheck (class Arbitrary, Result(..), (==?))
import Test.Reference (reference)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.StateQueue (stateQueueTests)
import Test.Url (urlTest)


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
re4 = sym 'a' *> foldMany (singleton 'b') <* sym 'a'

re5 :: Re Char String
re5 = (sym 'a' <|> sym 'a' *> sym 'a') *> foldMany (singleton 'a')

re6 :: Re Char (List Int)
re6 = many (pure 3 <* sym 'a' <* sym 'a' <* sym 'a'
            <|> pure 1 <* sym 'a')

-- Regular expression from the weighted regexp paper.
re7 :: Re Char (Tuple (List (Tuple4 String Char String Char)) String)
re7 =
  let manyAOrB = foldMany (singleton 'a' <|> singleton 'b')
  in Tuple <$> many (tuple4 <$> manyAOrB
                            <*> sym 'c'
                            <*> manyAOrB
                            <*> sym 'c')
           <*> manyAOrB

re8 :: Re Char (Tuple String String)
re8 = Tuple <$> foldMany (singleton 'a' <|> singleton 'b')
            <*> foldMany (singleton 'b' <|> singleton 'c')

re9 :: Re Char String
re9 = foldMany (singleton 'a' <|> empty) <* sym 'b'

re10 :: Re Char String
re10 = foldFew (singleton 'a' <|> empty) <* sym 'b'

testMatchVsRef :: forall c b a. Eq b => Show b => Newtype c a =>
              (a -> c) ->  -- constructor (ignored, just to specify Newtype)
              Re a b ->
              Array c ->
              Result
testMatchVsRef _ re s = realResult ==? refResult where
  s' = map unwrap $ fromFoldable s
  refResult = reference re s'
  realResult = match re s'

testWithMatched :: Re Char String -> Array AB -> Result
testWithMatched re s =
  case (fromCharArray $ map unwrap s) =~ withMatched re of
    Nothing -> Success
    Just (Tuple x y) -> x ==? y


main :: forall e. Eff (QCRunnerEffects e) Unit
main = run [consoleReporter] $
  describe "Tests" $ do
    describe "StateQueue tests" do
      stateQueueTests
    describe "Url test" $ do
      urlTest
    describe "Expression test" $ do
      expressionTest
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
      describe "withMatched" $
        it "withMatched" $ quickCheck $
          testWithMatched (foldMany (str "a" <|> str "ba"))
    describe "Fixtures" $ do
      describe "Matching vs reference" $ do
        it "re0" $
          ("a" =~ re0) `shouldEqual` reference re0 "a"
        it "re9" $
          ("a" =~ re9) `shouldEqual` reference re9 "a"
      describe "findFirstPrefix" $ do
        it "t1" $
          findFirstPrefix (str "a" <|> str "ab") "abc" `shouldEqual`
            Just (Match { before: "", result: "a", after: "bc" })
        it "t2" $
          findFirstPrefix (str "ab" <|> str "a") "abc" `shouldEqual`
            Just (Match { before: "", result: "ab", after: "c" })
        it "t3" $
          findFirstPrefix (str "bc") "abc" `shouldEqual`
            Nothing
      describe "findFirstInfix" $ do
        it "t1" $
          findFirstInfix (str "a" <|> str "ab") "tabc" `shouldEqual`
            Just (Match { before: "t", result:  "a", after: "bc" })
        it "t2" $
          findFirstInfix (str "ab" <|> str "a") "tabc" `shouldEqual`
            Just (Match { before: "t", result: "ab", after: "c" })
      describe "findLongestPrefix" $ do
        it "t1" $
          findLongestPrefix (str "a" <|> str "ab") "abc" `shouldEqual`
            Just (Match { before: "", result: "ab", after: "c" })
        it "re9" $
          findLongestPrefix re9 "abc" `shouldEqual`
            Just (Match { before: "", result: "a", after: "c" })
        it "t2" $
          findLongestPrefix (str "ab" <|> str "a") "abc" `shouldEqual`
            Just (Match { before: "", result: "ab", after: "c" })
        it "t3" $
          findLongestPrefix (str "bc") "abc" `shouldEqual`
            Nothing
      describe "findLongestInfix" $ do
        it "t1" $
          findLongestInfix (str "a" <|> str "ab") "tabc" `shouldEqual`
            Just (Match { before: "t", result:  "ab", after: "c" })
        it "t2" $
          findLongestInfix (str "ab" <|> str "a") "tabc" `shouldEqual`
            Just (Match { before: "t", result: "ab", after: "c" })
        it "t3" $
          findLongestInfix (str "bc") "tabc" `shouldEqual`
            Just (Match { before: "ta", result: "bc", after: "" })
      describe "findShortestPrefix" $ do
        it "t1" $
          findShortestPrefix (str "a" <|> str "ab") "abc" `shouldEqual`
            Just (Match { before: "", result: "a", after: "bc" })
        it "t2" $
          findShortestPrefix (str "ab" <|> str "a") "abc" `shouldEqual`
            Just (Match { before: "", result: "a", after:  "bc" })
        it "t3" $
          findShortestPrefix (str "bc") "abc" `shouldEqual`
            Nothing
      describe "findShortestInfix" $ do
        it "t1" $
          findShortestInfix (str "a" <|> str "ab") "tabc" `shouldEqual`
            Just (Match { before: "t", result: "a", after: "bc" })
        it "t2" $
          findShortestInfix (str "ab" <|> str "a") "tabc" `shouldEqual`
            Just (Match { before:  "t", result: "a", after: "bc" })
        it "t3" $
          findShortestInfix (str "bc") "tabc" `shouldEqual`
            Just (Match { before:  "ta", result: "bc", after: "" })
      describe "replace" $ do
        it "t1" $
          replace ("x" <$ str "a" <|> "y" <$ str "ab") "tabc" `shouldEqual`
            "tyc"
        it "t2" $
          replace ("y" <$ str "ab" <|> "x" <$ str "a") "tabc" `shouldEqual`
            "tyc"
        it "t3" $
          replace ("x" <$ str "bc") "tabc" `shouldEqual`
            "tax"
        it "t4" $
          replace ("y" <$ str "a" <|> "x" <$ str "ab") "tacabc" `shouldEqual`
            "tycxc"
