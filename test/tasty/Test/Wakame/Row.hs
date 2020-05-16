module Test.Wakame.Row where

import Data.SOP.NP
import GHC.Generics
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Test.Utils
import Wakame.Generics ()
import Wakame.Row (Row (..), V (..), fromRow, toRow)


data Point = Point { x :: Double, y :: Double }
  deriving (Eq, Show, Generic)

prop_toRow :: (V '("x", Double), V '("y", Double)) -> Property
prop_toRow (x, y) =
  toRow (x, y) === x :* y :* Nil

prop_fromRow :: (V '("x", Double), V '("y", Double)) -> Property
prop_fromRow (x@(V x'), y@(V y')) =
  fromRow (x :* y :* Nil) === Point x' y'
