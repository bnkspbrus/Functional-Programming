module HW1.T4 where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ s Leaf = s
tfoldr f s (Branch _ l x r) = tfoldr f (f x (tfoldr f s r)) l

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []