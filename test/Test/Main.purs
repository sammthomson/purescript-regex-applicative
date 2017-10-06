module Test.Main where



import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Plus (empty)
import Data.List.Lazy (List, concat, fromFoldable, nil, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Regex.Applicative (RE, few, many, findFirstPrefix, string, sym, withMatched, (=~))
import Data.String (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, Unit, discard, map, pure, show, unit, ($), (*>), (+), (<$>), (<*), (<*>), (<>))
import Test.QuickCheck (Result(..), (==?))
import Test.Reference (reference)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

-- Small alphabets as SmallCheck's series
newtype A = A Char
derive instance newtypeA :: Newtype A _
instance showA :: Show A where
  show (A a) = "(A " <> show a <> ")"

-- instance monadSerialA :: Monad m => Serial m A where
--   series = cons0 $ A 'a'

newtype AB = AB Char
derive instance newtypeAB :: Newtype AB _
instance showAB :: Show AB where
  show (AB x) = "(AB " <> show x <> ")"

-- instance monadSerialAB :: Monad m => Serial m AB where
--   series = cons0 (AB 'a') \/ cons0 (AB 'b')

newtype ABC = ABC Char
derive instance newtypeABC :: Newtype ABC _
instance showABC :: Show ABC where
  show (ABC x) = "(ABC " <> show x <> ")"

-- instance monadSerialABC :: Monad m => Serial m ABC where
--   series = cons0 (ABC 'a') \/ cons0 (ABC 'b') \/ cons0 (ABC 'c')

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

-- prop :: RE Char a -> (a -> b)
prop :: forall c b a. Eq b => Show b => RE a b -> (c -> a) -> List c -> Result
prop re f s =
  let
    fs = map f s
  in
    reference re fs ==? (fs =~ re)

prop_withMatched :: List A -> Result
prop_withMatched =
  let
    -- re = withMatched $ string "a" <|> string "ba"
    re = withMatched $ many (string "a" <|> string "ba")
  in
    \str ->
      case map unwrap str =~ re of
        Nothing -> Success
        Just (Tuple x y) -> concat x ==? y

-- -- data Test = Test String 
-- -- data TestGroup = TestGroup String (List Test)

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
    --   it "re1" $ quickCheck $ prop re1 unwrap
    --   it "re2" $ quickCheck $ prop re2 unwrap
    --   it "re3" $ quickCheck $ prop re3 unwrap
    --   it "re4" $ quickCheck $ prop re4 unwrap
    --   it "re5" $ quickCheck $ prop re5 unwrap
    --   it "re6" $ quickCheck $ prop re6 unwrap
    --   it "re7" $ quickCheck $ prop re7 unwrap
    --   it "re8" $ quickCheck $ prop re8 unwrap
    -- describe "Recognition vs parsing" $ do
    --   it "re1" $ quickCheck $ testRecognitionAgainstParsing re1 unwrap
    --   it "re2" $ quickCheck $ testRecognitionAgainstParsing re2 unwrap
    --   it "re3" $ quickCheck $ testRecognitionAgainstParsing re3 unwrap
    --   it "re4" $ quickCheck $ testRecognitionAgainstParsing re4 unwrap
    --   it "re5" $ quickCheck $ testRecognitionAgainstParsing re5 unwrap
    --   it "re6" $ quickCheck $ testRecognitionAgainstParsing re6 unwrap
    --   it "re7" $ quickCheck $ testRecognitionAgainstParsing re7 unwrap
    --   it "re8" $ quickCheck $ testRecognitionAgainstParsing re8 unwrap
    --   it "re8" $ quickCheck $ testRecognitionAgainstParsing re9 unwrap
    --   it "re8" $ quickCheck $ testRecognitionAgainstParsing re10 unwrap
    --   it "withMatched" $ quickCheck prop_withMatched
    describe "Tests for matching functions" $ do
      describe "findFirstPrefix" $ do
        it "t1" $ quickCheck $
            (findFirstPrefix (string "a" <|> string "ab") (fromFoldable $ toCharArray "abc")) ==?
            (Just (Tuple ('a' : nil) ('b' : 'c' : nil)))
        it "t2" $ quickCheck $
            (findFirstPrefix (string "ab" <|> string "a") (fromFoldable $ toCharArray "abc")) ==?
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
