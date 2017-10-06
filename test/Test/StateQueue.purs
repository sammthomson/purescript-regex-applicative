module Test.StateQueue where

import Control.Applicative ((<$>), (<*>))
import Data.Regex.Applicative.StateQueue (StateQueue, getElements, insert, insertUnique, mkStateQueue)
import Control.Monad.Eff.Random (RANDOM)
import Data.List.Lazy (fromFoldable, last, length)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as S
import Prelude (Unit, discard, show, ($), (+), (-), (<<<), (<>), (==), (||))
import Test.QuickCheck (class Arbitrary, arbitrary, (<?>), (==?))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

-- fromElems :: Array (Tuple a (Maybe Int)) -> StateQueue a
-- fromElems = foldl f SQ.empty
--   where
--     f sq (x, mbKey) = maybe insert insertUnique mbKey x sq

size :: forall a. StateQueue a -> Int
size = length <<< getElements

newtype TestSQ a = TestSQ (StateQueue a)
derive instance newtypeTestSQ :: Newtype (TestSQ a) _

instance arbSQ :: Arbitrary a => Arbitrary (TestSQ a) where
  arbitrary = TestSQ <$>
                (mkStateQueue <$> (fromFoldable <$> (arbitrary :: Gen (Array a)))
                              <*> (S.fromFoldable <$> (arbitrary :: Gen (Array Int))))

-- instance serialSQ :: (Monad m, Serial m a) => Serial m (StateQueue a) where
--   series = fromElems <$> series

stateQueueTests :: forall e. Spec (random :: RANDOM | e) Unit
stateQueueTests =
  describe "StateQueue" do
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
    -- it "insert doesn't affect insertUnique" $ quickCheck $
    --     \(i :: Int) -> exists $ \(TestSQ sq) ->
    --       let sq' = insert i sq
    --       in insertUnique i i sq' /= sq'
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
