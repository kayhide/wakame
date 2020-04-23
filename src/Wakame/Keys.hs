{-# LANGUAGE UndecidableInstances #-}
module Wakame.Keys where

import Prelude

import Data.Proxy
import Data.String (IsString (..))
import GHC.Generics
import GHC.TypeLits
import Wakame.Rec (Keyed (..))


-- $setup
-- >>> import Wakame.Examples


-- | Function to retrieve key values
-- >>> keys pt
-- ["x","y"]
--
-- Return value can be anything which has @IsString@ instance.
-- >>> import Data.Functor.Identity (Identity)
-- >>> keys pt :: [Identity String]
-- [Identity "x",Identity "y"]
keys :: (IsString s, Generic a, Keys' (Rep a) s) => a -> [s]
keys = keys' . from


class IsString s => Keys' f s where
  keys' :: f a -> [s]

instance Keys' a s => Keys' (D1 f a) s where
  keys' (M1 x) = keys' x

instance Keys' a s => Keys' (C1 f a) s where
  keys' (M1 x) = keys' x

instance (Keys' a s, Keys' b s) => Keys' (a :*: b) s where
  keys' (x :*: y) = keys' x <> keys' y

instance (KnownSymbol key, IsString s) => Keys' (S1 ('MetaSel ('Just key) su ss ds) a) s where
  keys' _ = [fromString $ symbolVal (Proxy @key)]

instance (KnownSymbol key, IsString s) => Keys' (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) s where
  keys' _ = [fromString $ symbolVal (Proxy @key)]
