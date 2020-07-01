module Test.Wakame.Rec where

import GHC.Generics
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Test.Utils
import Wakame.Examples (Point (..))
import Wakame.Rec (Keyed (..), Rec (..), fromRec, toRec)


prop_toRec :: (Keyed "x" Double, Keyed "y" Double) -> Property
prop_toRec (x, y) =
  toRec (x, y) === RCons x (RCons y RNil)

prop_fromRec :: (Keyed "x" Double, Keyed "y" Double) -> Property
prop_fromRec (x@(Keyed x'), y@(Keyed y')) =
  fromRec (RCons x (RCons y RNil)) === Point x' y'
