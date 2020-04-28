{-# LANGUAGE DuplicateRecordFields #-}
module Wakame.Examples.Functions where

import Prelude

import Control.Arrow (first, (***))
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Wakame


-- * Data types which is used in this Wakame.Examples module

data Point = Point { x :: Double, y :: Double }
  deriving (Eq, Show, Generic)

data Position = Position { x :: !Double, y :: !Double }
  deriving (Eq, Show, Generic)

data Point3d = Point3d { x :: Double, y :: Double, z :: Double }
  deriving (Eq, Show, Generic)

data Tniop = Tniop { y :: Double, x :: Double }
  deriving (Eq, Show, Generic)


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
-- >>> fromRec @Point $ toRec pt
-- Point {x = 1.2, y = 8.3}

-- | Converting Point to Point3d by adding z field
-- >>> fromRec @Point3d $ union (toRec pt) (toRec (Keyed @"z" 42.0))
-- Point3d {x = 1.2, y = 8.3, z = 42.0}


-- | Interfacing optional fields
--
-- `f` fills absent fileds with `0.0`.
-- >>> f $ toRec (Keyed @"x" 3.5)
-- Point {x = 3.5, y = 0.0}
--
-- Ignores extra fields.
-- >>> f $ toRec (Keyed @"y" 4.3, Keyed @"z" 1.6)
-- Point {x = 0.0, y = 4.3}
--
-- Converts from another data type.
-- >>> f $ toRec $ Point3d 3.5 4.3 1.6
-- Point {x = 3.5, y = 4.3}
--
-- Returns fully default value when `()` is given.
-- >>> f $ toRec ()
-- Point {x = 0.0, y = 0.0}
--
-- Works nicely with data whose fields order is not the same.
-- >>> f $ toRec $ Tniop 4.3 3.5
-- Point {x = 3.5, y = 4.3}
f ::
  ( Union props PointRow props'
  , Nub props' PointRow
  ) => Rec props -> Point
f props = fromRec $ nub $ union props def
  where
    def :: Rec PointRow
    def = toRec $ Point 0.0 0.0

type PointRow = RecType (Rep Point)


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
  ( Union (RecType (Rep b)) (RecType (Rep a)) s
  , Nub s (RecType (Rep c))
  , Generic a
  , Generic b
  , Generic c
  , IsRec (Rep a)
  , IsRec (Rep b)
  , IsRec (Rep c)
  ) => a -> b -> c
g x y = fromRec $ nub $ union (toRec y) (toRec x)


-- | Rejecting certain fields
--
-- `h` is almost the same as `g` but rejecting an argument with field "y".
-- >>> h (Point 3.5 4.3) (Timestamp "2020-04-01" "2020-04-02") :: Point
-- ...
-- ... Couldn't match type ...
-- ...

h ::
  ( Union (RecType (Rep b)) (RecType (Rep a)) s
  , Nub s (RecType (Rep c))
  , Generic a
  , Generic b
  , Generic c
  , IsRec (Rep a)
  , IsRec (Rep b)
  , IsRec (Rep c)
  , Lacks "y" (RecType (Rep a))
  ) => a -> b -> c
h x y = fromRec $ nub $ union (toRec y) (toRec x)
