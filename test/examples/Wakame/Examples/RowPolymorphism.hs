{-# LANGUAGE AllowAmbiguousTypes #-}
module Wakame.Examples.RowPolymorphism where

import Prelude

import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, Symbol)
import Wakame


-- * Row polymorphism
--
-- Theoretically, all row-polymorphic operations can be described as a
-- composition of the 3 basic functions:
--
-- - select_l :: {l} -> {l :: a | r} -> a
-- - add_l :: {l} -> a -> {absent(l) :: a | r} -> {l :: a | r}
-- - remove_l :: {l} -> {l :: a | r} -> {absent(l) :: a | r}
--
-- This module shows how to implement these functions.


select ::
  forall (l :: Symbol) (a :: Type) r.
  ( IsRow r
  , KnownSymbol l
  , Nub (Of r) '[ '(l, a) ]
  ) =>
  r -> a
select x =
  unV (fromRow $ nub $ toRow x :: V '(l, a))


add ::
  forall (l :: Symbol) (a :: Type) r r'.
  ( IsRow r
  , IsRow r'
  , KnownSymbol l
  , Lacks l (Of r)
  , Merge '[ '(l, a) ] (Of r) (Of r')
  ) =>
  a -> r -> r'
add e x =
  fromRow $ merge (toRow $ keyed @l e) (toRow x)


remove ::
  forall (l :: Symbol) r r'.
  ( IsRow r
  , IsRow r'
  , KnownSymbol l
  , Lacks l (Of r')
  , Nub (Of r) (Of r')
  ) =>
  r -> r'
remove x =
  fromRow $ nub $ toRow x
