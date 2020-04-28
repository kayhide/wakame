{-# LANGUAGE UndecidableInstances #-}
module Wakame.Union where

import Prelude

import Control.Arrow (first)
import GHC.Generics
import Wakame.Utils (Append (..))
import Wakame.Rec (Rec (..))


-- $setup
-- >>> import Wakame
-- >>> import Wakame.Examples


-- | Typeclass for composing fields
--
-- >>> union (toRec pt) (toRec (Keyed @"z" 42.0))
-- x: 1.2, y: 8.3, z: 42.0, _
--
-- >>> ununion (toRec pt) :: (Rec '[ '("x", Double)], Rec '[ '("y", Double)])
-- (x: 1.2, _,y: 8.3, _)
-- >>> ununion (toRec pt) :: (Rec '[], Rec '[ '("x", Double), '("y", Double)])
-- (_,x: 1.2, y: 8.3, _)
class Union l r u | l r -> u where
  union :: Rec l -> Rec r -> Rec u
  ununion :: Rec u -> (Rec l, Rec r)

instance Union '[] r r where
  union _ r = r
  ununion x = (RNil, x)

instance (Union l r u, u ~ Append l r) => Union (x ': l) r (x ': u) where
  union (RCons x xs) r = RCons x $ union xs r
  ununion (RCons x xs) = first (RCons x) $ ununion xs
