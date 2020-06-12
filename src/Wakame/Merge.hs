{-# LANGUAGE UndecidableInstances #-}
module Wakame.Merge where

import Prelude

import Wakame.Nub (Nub (..))
import Wakame.Row (Row)
import Wakame.Union (Union (..))
import Wakame.Utils (type (++))


-- $setup
-- >>> import GHC.Generics
-- >>> import Wakame
-- >>> data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)
-- >>> pt = Point 1.2 8.3


-- | Typeclass of the combination of Union and Nub
-- Use this typeclass and function if the intermediate row is not necessary.
--
-- >>> merge (toRow $ keyed @"x" 42.0) (toRow pt) :: Row (Of Point)
-- (x: 42.0) :* (y: 8.3) :* Nil
class (Union l r (l ++ r), Nub (l ++ r) m) => Merge l r m where
  merge :: Row l -> Row r -> Row m

instance (Union l r (l ++ r), Nub (l ++ r) m) => Merge l r m where
  merge x y = nub $ union x y
