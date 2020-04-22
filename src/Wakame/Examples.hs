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


-- | An instance of @Point@ data type.
-- Let's review the basic functionality of @Generic@.
--
-- `from` function converts to its generic representation.
-- >>> from pt
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1.2}} :*: M1 {unM1 = K1 {unK1 = 8.3}}}}
--
-- We can construct a representation value by hand.
-- >>> rep = M1 $ M1 $ M1 (K1 1.2) :*: M1 (K1 8.3)
--
-- From the representation value, `to` function converts to @Record@.
-- >>> to rep :: Point
-- Point {x = 1.2, y = 8.3}
--
-- Since @Position@ has the same representation, `to` works as well.
-- >>> to rep :: Position
-- Position {x = 1.2, y = 8.3}
--
-- And even so to Tuple.
-- >>> to rep :: (Double, Double)
-- (1.2,8.3)
pt :: Point
pt = Point 1.2 8.3



-- * Examples of Record value manipulation

-- | Round trip of Point to/from Rec
-- >>> to @Point $ fromRec $ toRec $ from pt
-- Point {x = 1.2, y = 8.3}

-- | Converting Point to Point3d by adding z field
-- >>> to @Point3d $ fromRec $ union (toRec $ from pt) (toRec $ from (Keyed @"z" 42.0))
-- Point3d {x = 1.2, y = 8.3, z = 42.0}


-- | Interfacing optional fields
--
-- `f` fills absent fileds with `0.0`.
-- >>> f $ toRec $ from (Keyed @"x" 3.5)
-- Point {x = 3.5, y = 0.0}
--
-- Ignores extra fields.
-- >>> f $ toRec $ from (Keyed @"y" 4.3, Keyed @"z" 1.6)
-- Point {x = 0.0, y = 4.3}
--
-- Converts from another data type.
-- >>> f $ toRec $ from $ Point3d 3.5 4.3 1.6
-- Point {x = 3.5, y = 4.3}
--
-- Returns fully default value when `()` is given.
-- >>> f $ toRec $ from ()
-- Point {x = 0.0, y = 0.0}
--
-- Works nicely with data whose fields order is not the same.
-- >>> f $ toRec $ from $ Tniop 4.3 3.5
-- Point {x = 3.5, y = 4.3}
f ::
  ( Union props PointRow props'
  , Nub props' PointRow
  ) => Rec props -> Point
f props = to $ fromRec $ nub $ union props def
  where
    def :: Rec PointRow
    def = toRec $ from $ Point 0.0 0.0

type PointRow = RowTy (Rep Point)
