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
