{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLabels #-}
module Wakame.Labels where

import Prelude

import Wakame.Row (Keyed, V(..))
import GHC.OverloadedLabels


-- |
-- >>> :set -XOverloadedLabels
-- >>> import Wakame
-- >>> #x True :: Keyed "x" Bool
-- (x: True)
instance IsLabel k (v -> Keyed k v) where
  fromLabel = V

keyed' :: IsLabel k (v -> Keyed k v) => (v -> Keyed k v) -> v -> Keyed k v
keyed' f = f

-- This does not infer
-- foo = keyed' #x True

-- |
-- >>> import Wakame
-- >>> toRow (keyed' #x 1.5) :: Row '[ '("x", Double)]
-- (x: 1.5) :* Nil

-- |
-- >>> import GHC.Generics
-- >>> import Wakame
-- >>> data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)
-- >>> fromRow $ toRow (keyed' #x 1.5, keyed' #y 2.3) :: Point
-- Point {x = 1.5, y = 2.3}
