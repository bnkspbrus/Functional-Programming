module HW1.T3 where

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)

data Meta = Meta
  { getSize :: Int,
    getDepth :: Int
  }

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch m _ _ _) = getSize m

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch m _ _ _) = getDepth m

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ l x r)
  | a < x = tmember a l
  | a == x = True
  | otherwise = tmember a r

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l x r = Branch Meta {getSize = tsize l + tsize r + 1, getDepth = max (tdepth l) (tdepth r) + 1} l x r

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch Meta {getSize = 1, getDepth = 1} Leaf x Leaf
tinsert x b@(Branch _ l a r)
  | x == a = b
  | x < a = mkBranch (tinsert x l) a r
  | otherwise = mkBranch l a (tinsert x r)

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
