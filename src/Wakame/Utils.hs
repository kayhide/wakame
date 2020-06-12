module Wakame.Utils where

import Prelude


-- |
-- >>> :kind! '[Bool, Int] ++ '[]
-- '[Bool, Int] ++ '[] :: [*]
-- = '[Bool, Int]
-- >>> :kind! '[Bool, Int] ++ '[Char, Word]
-- '[Bool, Int] ++ '[Char, Word] :: [*]
-- = '[Bool, Int, Char, Word]
type family (++) (a :: [k]) (b :: [k]) :: [k]
type instance (++) '[] ys = ys
type instance (++) (x ': xs) ys = x ': (xs ++ ys)

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
