module Wakame.Nub where

import Prelude

import Wakame.Row (NP (..), Row, V)


-- $setup
-- >>> import Wakame


-- | Typeclass for reshaping fields
-- `nub` function eliminates duplicate fileds.
-- When duplication, the first element takes precedence.
-- It also reorders fields so to match the return type.
--
-- >>> toRow (keyed @"x" 42.0, keyed @"x" 56.4)
-- (x: 42.0) :* (x: 56.4) :* Nil
-- >>> nub $ toRow (keyed @"x" 42.0, keyed @"x" 56.4) :: Row '[ '("x", Double)]
-- (x: 42.0) :* Nil
class Nub s t where
  nub :: Row s -> Row t

instance Nub s '[] where
  nub _ = Nil

instance (Nub s t, HasField s k v) => Nub s ('(k, v) ': t) where
  nub x = getField x :* nub x


-- | Typeclass to pick a first matched field
class HasField r k v where
  getField :: Row r -> V '(k, v)

instance {-# OVERLAPS #-} HasField ('(k, v) ': rs) k v where
  getField (x :* _) = x

instance HasField rs k v => HasField (r ': rs) k v where
  getField (_ :* xs) = getField xs
