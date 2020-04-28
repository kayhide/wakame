{-# LANGUAGE UndecidableInstances #-}
module Wakame.Generic where

import Prelude

import Control.Arrow ((***))
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Wakame.Rec (FIELD, IsRec (..), Keyed (..), Rec (..))
import Wakame.Union (Union (..))
import Wakame.Utils (Append (..))


-- $setup
-- >>> import Wakame.Examples

-- | Instances of generic reps to/from @Rec@
-- >>> :kind! RecType (Rep Point)
-- RecType (Rep Point) :: [(Symbol, *)]
-- = '[ '("x", Double), '("y", Double)]
--
-- >>> toRec' $ from $ Point 1.2 8.3
-- x: 1.2, y: 8.3, _
-- >>> to @Point $ fromRec' $ (RCons (Keyed @"x" 1.2) $ RCons (Keyed @"y" 8.3) RNil)
-- Point {x = 1.2, y = 8.3}
instance IsRec U1 where
  type RecType U1 = '[]
  fromRec' = const U1
  toRec' = const RNil

instance IsRec f => IsRec (D1 i f) where
  type RecType (D1 i f) = RecType f
  fromRec' = M1 . fromRec'
  toRec' (M1 x) = toRec' x

instance IsRec f => IsRec (C1 i f) where
  type RecType (C1 i f) = RecType f
  fromRec' = M1 . fromRec'
  toRec' (M1 x) = toRec' x

instance (IsRec a, IsRec b, l ~ RecType a, r ~ RecType b, u ~ Append l r, Union l r u) => IsRec (a :*: b) where
  type RecType (a :*: b) = Append (RecType a) (RecType b)
  fromRec' = uncurry (:*:) . (fromRec' *** fromRec') . ununion
  toRec' (x :*: y) = union (toRec' x) (toRec' y)

instance IsRec (S1 ('MetaSel ('Just (key :: Symbol)) su ss ds) (Rec0 (a :: Type))) where
  type RecType (S1 ('MetaSel ('Just key) su ss ds) (Rec0 a)) = '[ '(key, a) ]
  fromRec' (RCons (Keyed x) RNil) = M1 $ K1 x
  toRec' (M1 (K1 x)) = RCons (Keyed x) RNil

instance IsRec (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  type RecType (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) = '[ '(key, a) ]
  fromRec' (RCons x RNil) = M1 $ K1 x
  toRec' (M1 (K1 x)) = RCons x RNil
