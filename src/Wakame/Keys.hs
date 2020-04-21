{-# LANGUAGE UndecidableInstances #-}
module Wakame.Keys where

import Prelude

import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Wakame.Rec (Keyed (..))


-- $setup
-- >>> import Wakame.Examples


-- |
-- >>> keys pt
-- ["x","y"]
keys :: (Generic a, Keys' (Rep a)) => a -> [String]
keys = keys' . from


class Keys' f where
  keys' :: f a -> [String]

instance Keys' a => Keys' (D1 f a) where
  keys' (M1 x) = keys' x

instance Keys' a => Keys' (C1 f a) where
  keys' (M1 x) = keys' x

instance (Keys' a, Keys' b) => Keys' (a :*: b) where
  keys' (x :*: y) = keys' x <> keys' y

instance KnownSymbol key => Keys' (S1 ('MetaSel ('Just key) su ss ds) a) where
  keys' _ = [symbolVal (Proxy @key)]

instance KnownSymbol key => Keys' (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  keys' _ = [symbolVal (Proxy @key)]
