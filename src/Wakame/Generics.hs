{-# LANGUAGE UndecidableInstances #-}
module Wakame.Generics where

import Prelude

import Control.Arrow ((***))
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Wakame.Rec (FIELD, IsRec (..), Keyed (..), Rec (..))
import Wakame.Union (Union (..))
import Wakame.Utils (Append (..))


-- $setup
-- >>> data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)


-- | Instance of @IsRec@ over generic rep
-- >>> :kind! RowOf Point
-- RowOf Point :: [(Symbol, *)]
-- = '[ '("x", Double), '("y", Double)]
--
-- >>> toRec' $ from $ Point 1.2 8.3
-- x: 1.2, y: 8.3, _
-- >>> to @Point $ fromRec' $ (RCons (Keyed @"x" 1.2) $ RCons (Keyed @"y" 8.3) RNil)
-- Point {x = 1.2, y = 8.3}
instance (Generic a, IsRec' (Rep a)) => IsRec a where
  type RowOf a = RowOf' (Rep a)
  fromRec = to . fromRec'
  toRec = toRec' . from


-- * Internal

class IsRec' f where
  type RowOf' f :: [FIELD]
  fromRec' :: Rec (RowOf' f) -> f a
  toRec' :: f a -> Rec (RowOf' f)

instance IsRec' U1 where
  type RowOf' U1 = '[]
  fromRec' = const U1
  toRec' = const RNil

instance IsRec' f => IsRec' (D1 i f) where
  type RowOf' (D1 i f) = RowOf' f
  fromRec' = M1 . fromRec'
  toRec' (M1 x) = toRec' x

instance IsRec' f => IsRec' (C1 i f) where
  type RowOf' (C1 i f) = RowOf' f
  fromRec' = M1 . fromRec'
  toRec' (M1 x) = toRec' x

instance (IsRec' a, IsRec' b, l ~ RowOf' a, r ~ RowOf' b, u ~ Append l r, Union l r u) => IsRec' (a :*: b) where
  type RowOf' (a :*: b) = Append (RowOf' a) (RowOf' b)
  fromRec' = uncurry (:*:) . (fromRec' *** fromRec') . ununion
  toRec' (x :*: y) = union (toRec' x) (toRec' y)

instance IsRec' (S1 ('MetaSel ('Just (key :: Symbol)) su ss ds) (Rec0 (a :: Type))) where
  type RowOf' (S1 ('MetaSel ('Just key) su ss ds) (Rec0 a)) = '[ '(key, a) ]
  fromRec' (RCons (Keyed x) RNil) = M1 $ K1 x
  toRec' (M1 (K1 x)) = RCons (Keyed x) RNil

instance IsRec' (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  type RowOf' (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) = '[ '(key, a) ]
  fromRec' (RCons x RNil) = M1 $ K1 x
  toRec' (M1 (K1 x)) = RCons x RNil
