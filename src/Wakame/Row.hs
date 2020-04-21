{-# LANGUAGE UndecidableInstances #-}
module Wakame.Row where

import Prelude

import Control.Arrow ((***))
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Wakame.Utils (Append (..))
import Wakame.Rec (FIELD, Keyed (..), Rec (..))
import Wakame.Union (Union (..))


-- $setup
-- >>> import Wakame.Examples


-- |
-- >>> :kind! RowTy (Rep Point)
-- RowTy (Rep Point) :: [(Symbol, *)]
-- = '[ '("x", Double), '("y", Double)]
class Row f where
  type RowTy f :: [FIELD]
  toRec :: f a -> Rec (RowTy f)
  fromRec :: Rec (RowTy f) -> f a

instance Row f => Row (D1 i f) where
  type RowTy (D1 i f) = RowTy f
  toRec (M1 x) = toRec x
  fromRec = M1 . fromRec

instance Row f => Row (C1 i f) where
  type RowTy (C1 i f) = RowTy f
  toRec (M1 x) = toRec x
  fromRec = M1 . fromRec

instance (Row a, Row b, l ~ RowTy a, r ~ RowTy b, u ~ Append l r, Union l r u) => Row (a :*: b) where
  type RowTy (a :*: b) = Append (RowTy a) (RowTy b)
  toRec (x :*: y) = union (toRec x) (toRec y)
  fromRec = uncurry (:*:) . (fromRec *** fromRec) . ununion

instance Row (S1 ('MetaSel ('Just (key :: Symbol)) su ss ds) (Rec0 (a :: Type))) where
  type RowTy (S1 ('MetaSel ('Just key) su ss ds) (Rec0 a)) = '[ '(key, a) ]
  toRec (M1 (K1 x)) = RCons (Keyed x) RNil
  fromRec (RCons (Keyed x) RNil) = M1 $ K1 x

instance Row (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  type RowTy (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) = '[ '(key, a) ]
  toRec (M1 (K1 x)) = RCons x RNil
  fromRec (RCons x RNil) = M1 $ K1 x
