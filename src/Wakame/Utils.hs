module Wakame.Utils where

import Prelude


-- |
-- >>> :kind! Append '[Bool, Int] '[]
-- Append '[Bool, Int] '[] :: [*]
-- = '[Bool, Int]
-- >>> :kind! Append '[Bool, Int] '[Char, Word]
-- Append '[Bool, Int] '[Char, Word] :: [*]
-- = '[Bool, Int, Char, Word]
type family Append (a :: [k]) (b :: [k]) :: [k]
type instance Append '[] ys = ys
type instance Append (x ': xs) ys = x ': Append xs ys
