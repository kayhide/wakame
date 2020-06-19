{-# LANGUAGE DuplicateRecordFields #-}
module Wakame.Examples.Functions where

import Prelude

import GHC.Generics
import Wakame


-- * Data types which is used in this module

data Point = Point { x :: Double, y :: Double }
  deriving (Eq, Show, Generic)

data Position = Position { x :: !Double, y :: !Double }
  deriving (Eq, Show, Generic)

data Point3d = Point3d { x :: Double, y :: Double, z :: Double }
  deriving (Eq, Show, Generic)

data Tniop = Tniop { y :: Double, x :: Double }
  deriving (Eq, Show, Generic)


-- | An instance of @Point@ data type.
pt :: Point
pt = Point 1.2 8.3

-- | Review of @Generic@
-- Let's review the basic functionality of an instance of @Generic@ typeclass.
--
-- `from` function converts a data to its generic representation.
-- >>> from pt
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1.2}} :*: M1 {unM1 = K1 {unK1 = 8.3}}}}
--
-- You can construct a representation value by hand.
-- >>> rep = M1 $ M1 $ M1 (K1 1.2) :*: M1 (K1 8.3)
--
-- From the representation value, `to` function converts back to @Point@.
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
--
-- It is sensitive to the order of elements.
-- >>> to rep :: Tniop
-- Tniop {y = 1.2, x = 8.3}



-- * Examples of record value manipulation

-- | Round trip of Point to/from Row
-- >>> fromRow @Point $ toRow pt
-- Point {x = 1.2, y = 8.3}

-- | Converting Point to Point3d by adding z field
-- >>> fromRow @Point3d $ union (toRow pt) (toRow (keyed @"z" 42.0))
-- Point3d {x = 1.2, y = 8.3, z = 42.0}


-- | Interfacing optional fields
--
-- `f` fills absent fileds with `0.0`.
-- >>> f $ toRow (keyed @"x" 3.5)
-- Point {x = 3.5, y = 0.0}
--
-- Ignores extra fields.
-- >>> f $ toRow (keyed @"y" 4.3, keyed @"z" 1.6)
-- Point {x = 0.0, y = 4.3}
--
-- Converts from another type of data.
-- >>> f $ toRow $ Point3d 3.5 4.3 1.6
-- Point {x = 3.5, y = 4.3}
--
-- Returns fully default value when `()` is given.
-- >>> f $ toRow ()
-- Point {x = 0.0, y = 0.0}
--
-- Works nicely no matter how the order of fields is.
-- >>> f $ toRow $ Tniop 4.3 3.5
-- Point {x = 3.5, y = 4.3}
f ::
  ( Merge props OfPoint OfPoint
  ) => Row props -> Point
f props = fromRow $ merge props def
  where
    def :: Row OfPoint
    def = toRow $ Point 0.0 0.0

type OfPoint = Of Point


-- | Filling common fields if existing
--
-- >>> g (Person "Luke" "1979-01-01") (Timestamp "2020-04-01" "2020-04-02") :: Person
-- Person {name = "Luke", created_at = "2020-04-01"}
--
-- >>> g (Point 3.5 4.3) (Timestamp "2020-04-01" "2020-04-02") :: Point
-- Point {x = 3.5, y = 4.3}

data Timestamp =
  Timestamp
  { created_at :: String
  , updated_at :: String
  }
  deriving (Eq, Show, Generic)

data Person =
  Person
  { name       :: String
  , created_at :: String
  }
  deriving (Eq, Show, Generic)

g ::
  ( IsRow a
  , IsRow b
  , IsRow c
  , Merge (Of b) (Of a) (Of c)
  ) => a -> b -> c
g x y = fromRow $ nub $ union (toRow y) (toRow x)


-- | Rejecting certain fields
--
-- `h` is the same as `g` except rejecting the first argument of having a field "y".
-- >>> h (Point 3.5 4.3) (Timestamp "2020-04-01" "2020-04-02") :: Point
-- ...
-- ... Couldn't match type ...
-- ...

h ::
  ( IsRow a
  , IsRow b
  , IsRow c
  , Merge (Of b) (Of a) (Of c)
  , Lacks "y" (Of a)
  ) => a -> b -> c
h x y = fromRow $ nub $ union (toRow y) (toRow x)
