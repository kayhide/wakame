{-# LANGUAGE UndecidableInstances #-}
module Wakame.Union where

import Prelude

import Control.Arrow (first)
import Wakame.Row (NP (..), Row)
import Wakame.Utils (type (++))


-- $setup
-- >>> import GHC.Generics
-- >>> import Wakame
-- >>> data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)
-- >>> pt = Point 1.2 8.3


-- | Typeclass for composing fields
--
-- >>> union (toRow pt) (toRow (keyed @"z" 42.0))
-- (x: 1.2) :* (y: 8.3) :* (z: 42.0) :* Nil
--
-- >>> ununion (toRow pt) :: (Row '[ '("x", Double)], Row '[ '("y", Double)])
-- ((x: 1.2) :* Nil,(y: 8.3) :* Nil)
-- >>> ununion (toRow pt) :: (Row '[], Row '[ '("x", Double), '("y", Double)])
-- (Nil,(x: 1.2) :* (y: 8.3) :* Nil)
class (u ~ (l ++ r)) => Union l r u | l r -> u where
  union :: Row l -> Row r -> Row u
  ununion :: Row u -> (Row l, Row r)

instance Union '[] r r where
  union _ r = r
  ununion x = (Nil, x)

instance Union l r u => Union (x ': l) r (x ': u) where
  union (x :* xs) r = x :* union xs r
  ununion (x :* xs) = first (x :*) $ ununion xs
