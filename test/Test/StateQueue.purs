--------------------------------------------------------------------
-- |
-- | Module  : Test.StateQueue
-- | Copyright : (c) Roman Cheplyaka, 2011; Sam Thomson, 2017.
-- | License   : MIT
-- |
-- | Tests for `Data.Regex.Applicative.StateQueue`.
--------------------------------------------------------------------

module Test.StateQueue where

import Control.Monad.Eff.Random (RANDOM)
import Data.Foldable (foldl)
import Data.List.Lazy (last, length)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Regex.Applicative.StateQueue (StateQueue, empty, getElements, insert, insertUnique)
import Data.Tuple (Tuple(..))
import Prelude (Unit, discard, show, ($), (+), (-), (<$>), (<<<), (<>), (==), (||))
import Test.QuickCheck (class Arbitrary, arbitrary, (/=?), (<?>), (==?))
import Test.Spec (Spec, it)
import Test.Spec.QuickCheck (quickCheck)

size :: forall a. StateQueue a -> Int
size = length <<< getElements

newtype TestSQ a = TestSQ (StateQueue a)
derive instance newtypeTestSQ :: Newtype (TestSQ a) _

instance arbSQ :: Arbitrary a => Arbitrary (TestSQ a) where
  arbitrary = (TestSQ <<< fromElems) <$> arbitrary where
    fromElems :: Array (Tuple Int a) -> StateQueue a
    fromElems = foldl (\sq (Tuple i a) -> insertUnique i a sq) empty

stateQueueTests :: forall e. Spec (random :: RANDOM | e) Unit
stateQueueTests = do
  it "Insertion increments the # of elements" $ quickCheck $
      \(TestSQ sq) (i :: Int) -> size (insert i sq) ==? size sq + 1
  it "insertUnique increments the # of elements by 0 or 1" $ quickCheck $
      \(TestSQ sq) (i :: Int) ->
        let d = size (insertUnique i i sq) - size sq
        in (d == 0 || d == 1) <?> show d <> " not in [0, 1]"
  it "insertUnique is idempotent" $ quickCheck $
      \(TestSQ sq) (i :: Int) ->
        let f = insertUnique i i
        in f sq ==? (f <<< f) sq
  it "insert doesn't affect insertUnique" $ quickCheck $
      \(i :: Int) -> \(TestSQ sq) ->
        let sq' = insert i sq
        in insertUnique i i sq' /=? sq'
  it "insertUnique only cares about keys, not values" $ quickCheck $
      \(TestSQ sq) i j ->
        let sq' = insertUnique i i sq
        in insertUnique i j sq' ==? sq'
  it "insert puts in the back" $ quickCheck $
      \(TestSQ sq) (i :: Int) ->
        let sq' = insert i sq
        in last (getElements sq') ==? Just i
  it "insertUnique puts in the back" $ quickCheck $
      \(TestSQ sq) i ->
        let
          sq' = insertUnique i i sq
        in
          (sq' == sq || last (getElements sq') == Just i) <?>
            (show sq' <> " /= " <> show sq <> " && last sq' /= Just " <> show i)
