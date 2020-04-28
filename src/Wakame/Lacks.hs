module Wakame.Lacks where

import Prelude

import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits
import Wakame.Rec (FIELD, Rec (..))


-- | Typeclass to constrain not to have a certain key
class Lacks (k :: Symbol) (r :: [FIELD])
instance Lacks k '[]
instance (Lacks k r, KnownSymbol k', (k == k') ~ False) => Lacks k ('(k', a) ': r)
