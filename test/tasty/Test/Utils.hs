module Test.Utils where


import Test.QuickCheck (Arbitrary (..))
import Wakame.Rec (Keyed (..))


instance Arbitrary a => Arbitrary (Keyed k a) where
  arbitrary = Keyed <$> arbitrary
  shrink (Keyed x) = Keyed <$> shrink x
