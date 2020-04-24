module Wakame.Rec where

import Prelude

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Wakame.Utils (Append (..))


-- | Kind of field
type FIELD = (Symbol, Type)

-- * Data types

-- | Value tagged by @Symbol@
-- >>> x = Keyed @"x" @Double 3.5
-- >>> y = Keyed @"y" @Double 4.8
-- >>> :t RCons x (RCons y RNil)
-- RCons x (RCons y RNil) :: Rec '[ '("x", Double), '("y", Double)]
--
-- >>> (x, y)
-- (x: 3.5,y: 4.8)
--
-- It works with @keys@ function.
-- >>> import Wakame.Keys
-- >>> keys x
-- ["x"]
--
-- >>> keys (x, y)
-- ["x","y"]
--
newtype Keyed (k :: Symbol) (a :: Type) = Keyed a
  deriving (Eq)

instance (KnownSymbol k, Show a) => Show (Keyed k a) where
  show (Keyed x) = symbolVal (Proxy @k) <> ": " <> show x

instance Generic (Keyed k a) where
  type Rep (Keyed k a) = S1 ('MetaSel ('Just k) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 a)
  from (Keyed x) = M1 (K1 x)
  to (M1 (K1 x)) = Keyed x


-- | Heterogeneous associated list
data Rec (as :: [FIELD]) where
  RNil :: Rec '[]
  RCons :: Keyed k a -> Rec as -> Rec ('(k, a) ': as)

instance Eq (Rec '[]) where
  (==) _ _ = True

instance (KnownSymbol k, Eq a, Eq (Rec as)) => Eq (Rec ('(k, a) ': as)) where
  (==) (RCons x xs) (RCons y ys) = x == y && xs == ys

instance Show (Rec '[]) where
  show _ = "_"

instance (KnownSymbol k, Show a, Show (Rec as)) => Show (Rec ('(k, a) ': as)) where
  show (RCons x xs) = show x <> ", " <> show xs


-- | Typeclass of converting to/from @Rec@
class IsRec f where
  type RecType f :: [FIELD]
  fromRec' :: Rec (RecType f) -> f a
  toRec' :: f a -> Rec (RecType f)


-- * Functions to bypass generic reps

-- |
-- With Generic instances, a tuple of keyed values works.
--
-- >>> import Wakame.Generic ()
-- >>> x = Keyed @"x" @Double 3.5
-- >>> y = Keyed @"y" @Double 4.8
--
-- >>> fromRec (RCons x (RCons y RNil)) :: (Keyed "x" Double, Keyed "y" Double)
-- (x: 3.5,y: 4.8)
--
-- >>> toRec (x, y)
-- x: 3.5, y: 4.8, _
fromRec ::
  Generic a =>
  IsRec (Rep a) =>
  Rec (RecType (Rep a)) -> a
fromRec = to . fromRec'

toRec ::
  Generic a =>
  IsRec (Rep a) =>
  a -> Rec (RecType (Rep a))
toRec = toRec' . from
