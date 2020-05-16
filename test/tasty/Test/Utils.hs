module Test.Utils where


import Test.QuickCheck (Arbitrary (..))
import Wakame.Row (V (..))


instance Arbitrary a => Arbitrary (V '(k, a)) where
  arbitrary = V <$> arbitrary
  shrink (V x) = V <$> shrink x
