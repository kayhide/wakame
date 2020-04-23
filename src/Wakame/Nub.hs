module Wakame.Nub where

import Prelude

import GHC.Generics
import Wakame.Rec (Keyed (..), Rec (..))


-- $setup
-- >>> import Wakame


-- * Nub typeclass

-- |
-- >>> toRec (Keyed @"x" 42.0, Keyed @"x" 56.4)
-- x: 42.0, x: 56.4, _
-- >>> nub $ toRec (Keyed @"x" 42.0, Keyed @"x" 56.4) :: Rec '[ '("x", Double)]
-- x: 42.0, _
class Nub s t where
  nub :: Rec s -> Rec t

instance Nub s '[] where
  nub x = RNil

instance (Nub s t, HasField s k v) => Nub s ('(k, v) ': t) where
  nub x = RCons (getField x) $ nub x


class HasField r k v where
  getField :: Rec r -> Keyed k v

instance {-# OVERLAPS #-} HasField ('(k, v) ': rs) k v where
  getField (RCons x _) = x

instance HasField rs k v => HasField (r ': rs) k v where
  getField (RCons _ xs) = getField xs
