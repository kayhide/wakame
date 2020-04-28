{-# LANGUAGE UndecidableInstances #-}
module Wakame.Keys where

import Prelude

import Data.Proxy
import Data.String (IsString (..))
import GHC.Generics
import GHC.TypeLits
import Wakame.Rec (Keyed (..))


-- $setup
-- >>> import Wakame
-- >>> data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)
-- >>> pt = Point 1.2 8.3


-- | Function to retrieve key values
--
-- Return value can be anything which has @IsString@ instance.
-- >>> import Data.Functor.Identity (Identity)
-- >>> keys pt :: [Identity String]
-- [Identity "x",Identity "y"]
keys :: (IsString s, Generic a, Keys' s (Rep a)) => a -> [s]
keys = keys' . from


class IsString s => Keys' s f where
  keys' :: f a -> [s]

instance Keys' s a => Keys' s (D1 f a) where
  keys' (M1 x) = keys' x

instance Keys' s a => Keys' s (C1 f a) where
  keys' (M1 x) = keys' x

instance (Keys' s a, Keys' s b) => Keys' s (a :*: b) where
  keys' (x :*: y) = keys' x <> keys' y

instance (IsString s, KnownSymbol key) => Keys' s (S1 ('MetaSel ('Just key) su ss ds) a) where
  keys' _ = [fromString $ symbolVal (Proxy @key)]

instance (IsString s, KnownSymbol key) => Keys' s (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  keys' _ = [fromString $ symbolVal (Proxy @key)]
