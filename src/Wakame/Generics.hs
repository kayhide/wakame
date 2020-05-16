{-# LANGUAGE UndecidableInstances #-}
module Wakame.Generics where

import Prelude

import Control.Arrow ((***))
import Data.Kind
import Data.SOP.NP
import GHC.Generics
import GHC.TypeLits
import Wakame.Row (FIELD, IsRow (..), Row, V (..))
import Wakame.Union (Union (..))
import Wakame.Utils (type (++))


-- $setup
-- >>> import Wakame
-- >>> data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)


-- | Instance of @IsRow@ over generic rep
-- >>> :kind! Of Point
-- Of Point :: [(Symbol, *)]
-- = '[ '("x", Double), '("y", Double)]
--
-- >>> toRow' $ from $ Point 1.2 8.3
-- (x: 1.2) :* (y: 8.3) :* Nil
-- >>> to @Point $ fromRow' $ keyed @"x" 1.2 :* keyed @"y" 8.3 :* Nil
-- Point {x = 1.2, y = 8.3}


instance (Generic a, IsRow' (Rep a)) => IsRow a where
  type Of a = Of' (Rep a)
  fromRow = to . fromRow'
  toRow = toRow' . from


-- * Internal

class IsRow' f where
  type Of' f :: [FIELD]
  fromRow' :: Row (Of' f) -> f a
  toRow' :: f a -> Row (Of' f)

instance IsRow' U1 where
  type Of' U1 = '[]
  fromRow' = const U1
  toRow' = const Nil

instance IsRow' f => IsRow' (D1 i f) where
  type Of' (D1 i f) = Of' f
  fromRow' = M1 . fromRow'
  toRow' (M1 x) = toRow' x

instance IsRow' f => IsRow' (C1 i f) where
  type Of' (C1 i f) = Of' f
  fromRow' = M1 . fromRow'
  toRow' (M1 x) = toRow' x

instance (IsRow' a, IsRow' b, l ~ Of' a, r ~ Of' b, Union l r (l ++ r)) => IsRow' (a :*: b) where
  type Of' (a :*: b) = (Of' a) ++ (Of' b)
  fromRow' = uncurry (:*:) . (fromRow' *** fromRow') . ununion
  toRow' (x :*: y) = union (toRow' x) (toRow' y)

instance IsRow' (S1 ('MetaSel ('Just (key :: Symbol)) su ss ds) (Rec0 (a :: Type))) where
  type Of' (S1 ('MetaSel ('Just key) su ss ds) (Rec0 a)) = '[ '(key, a) ]
  fromRow' (V x :* Nil) = M1 $ K1 x
  toRow' (M1 (K1 x)) = V x :* Nil

instance IsRow' (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (V '(key, a)))) where
  type Of' (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (V '(key, a)))) = '[ '(key, a) ]
  fromRow' (x :* Nil) = M1 $ K1 x
  toRow' (M1 (K1 x)) = x :* Nil
