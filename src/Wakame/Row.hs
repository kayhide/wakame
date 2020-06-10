{-# LANGUAGE UndecidableInstances #-}
module Wakame.Row
  ( FIELD

  -- * Keyed value type and function
  , V (..)
  , Keyed
  , keyed

  -- * Row type
  , Row
  , IsRow (..)

  -- * Re-export from Data.SOP.NP
  , NP (..)
  ) where

import Prelude

import Data.Kind
import Data.Proxy
import Data.SOP.NP (NP (..))
import GHC.Generics
import GHC.TypeLits


-- | Kind of field
type FIELD = (Symbol, Type)

-- |
-- >>> :kind! Fst '(Bool, Char)
-- Fst '(Bool, Char) :: *
-- = Bool
type family Fst (p :: (a, b)) :: a
type instance Fst '(a, b) = a

-- |
-- >>> :kind! Snd '(Bool, Char)
-- Snd '(Bool, Char) :: *
-- = Char
type family Snd (p :: (a, b)) :: b
type instance Snd '(a, b) = b

-- |
-- >>> V 3 :: V '("x", Int)
-- (x: 3)
newtype V (p :: FIELD) = V (Snd p)

instance (KnownSymbol (Fst p), Show (Snd p)) => Show (V p) where
  show (V x) = "(" <> symbolVal (Proxy @(Fst p)) <> ": " <> show x <> ")"

instance (KnownSymbol (Fst p), Eq (Snd p)) => Eq (V p) where
  (==) (V x) (V y) = x == y

instance Generic (V '(k, v)) where
  type Rep (V '(k, v)) = S1 ('MetaSel ('Just k) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 v)
  from (V x) = M1 (K1 x)
  to (M1 (K1 x)) = V x

-- * Helper functions

type Keyed k v = V '(k, v)

keyed :: KnownSymbol k => v -> Keyed k v
keyed x = V x

-- * Row type

type Row as = NP V (as :: [FIELD])

-- | Typeclass of converting from/to @Row@
class IsRow (a :: Type) where
  type Of a :: [FIELD]
  fromRow :: Row (Of a) -> a
  toRow :: a -> Row (Of a)
