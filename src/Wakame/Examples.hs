{-# LANGUAGE DuplicateRecordFields #-}
module Wakame.Examples where

import Prelude

import Control.Arrow ((***), first)
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Wakame.Rec
import Wakame.Utils
import Wakame.Union
import Wakame.Nub
import Wakame.Row


-- * Data types which is used in this Wakame.Examples module

data Point = Point { x :: Double, y :: Double }
  deriving (Show, Generic)

data Position = Position { x :: !Double, y :: !Double }
  deriving (Show, Generic)

data Point3d = Point3d { x :: Double, y :: Double, z :: Double }
  deriving (Show, Generic)

data Tniop = Tniop { y :: Double, x :: Double }
  deriving (Show, Generic)


-- |
-- >>> from pt
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1.2}} :*: M1 {unM1 = K1 {unK1 = 8.3}}}}
--
-- >>> rep = M1 $ M1 $ M1 (K1 1.2) :*: M1 (K1 8.3)
-- >>> to rep :: Point
-- Point {x = 1.2, y = 8.3}
-- >>> to rep :: Position
-- Position {x = 1.2, y = 8.3}
-- >>> to rep :: (Double, Double)
-- (1.2,8.3)
pt :: Point
pt = Point 1.2 8.3



-- * Tests

-- | Round trip of Point to/from Rec
-- >>> to @Point $ fromRec $ toRec $ from pt
-- Point {x = 1.2, y = 8.3}

-- | Converting Point to Point3d by adding z field
-- >>> to @Point3d $ fromRec $ union (toRec $ from pt) (toRec $ from (Keyed @"z" 42.0))
-- Point3d {x = 1.2, y = 8.3, z = 42.0}


-- |
-- >>> f $ toRec $ from (Keyed @"x" 3.5)
-- Point {x = 3.5, y = 0.0}
--
-- >>> f $ toRec $ from (Keyed @"x" 3.5, Keyed @"y" 4.3)
-- Point {x = 3.5, y = 4.3}
--
-- >>> data Tniop = Tniop { y :: Double, x :: Double } deriving (Show, Generic)
-- >>> f $ toRec $ from $ Tniop 4.3 3.5
-- Point {x = 3.5, y = 4.3}
type PointRow = RowTy (Rep Point)

f ::
  ( Union props PointRow props'
  , Nub props' PointRow
  ) => Rec props -> Point
f props = to $ fromRec $ nub $ union props def
  where
    def :: Rec PointRow
    def = toRec $ from $ Point 0.0 0.0
