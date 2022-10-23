module HW1.T3
  ( Tree (..),
    Meta (Meta),
    getSize,
    getDepth,
    tsize,
    tdepth,
    tmember,
    tFromList,
    tinsert,
  )
where

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)

data Meta = Meta
  { getSize  :: Int,
    getDepth :: Int
  }

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf             = 0
tsize (Branch m _ _ _) = getSize m

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf             = 0
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

---- | Insert an element into the tree, O(log n)
--tinsert :: Ord a => a -> Tree a -> Tree a
--tinsert x Leaf = Branch Meta {getSize = 1, getDepth = 1} Leaf x Leaf
--tinsert x b@(Branch _ l a r)
--  | x == a = b
--  | x < a = mkBranch (tinsert x l) a r
--  | otherwise = mkBranch l a (tinsert x r)

bfactor :: Tree a -> Int
bfactor (Branch _ l _ r) = tdepth r - tdepth l
bfactor Leaf             = error "Lear doesn't have bfactor"

getLeft :: Tree a -> Tree a
getLeft (Branch _ l _ _) = l
getLeft Leaf             = error "Leaf doesn't have subtrees"

getRight :: Tree a -> Tree a
getRight (Branch _ _ _ r) = r
getRight Leaf             = error "Leaf doesn't have subtrees"

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

setLeft :: Tree a -> Tree a -> Tree a
setLeft (Branch _ _ x r) nl = mkBranch nl x r
setLeft Leaf _              = error "Leaf doesn't have subtrees"

setRight :: Tree a -> Tree a -> Tree a
setRight (Branch _ l x _) nr = mkBranch l x nr
setRight Leaf _              = error "Leaf doesn't have subtrees"

rotateRight :: Tree a -> Tree a
rotateRight p = let q = getLeft p in setRight q (setLeft p (getRight q))

rotateLeft :: Tree a -> Tree a
rotateLeft q = let p = getRight q in setLeft p (setRight q (getLeft p))

balance :: Tree a -> Tree a
balance p
  | bfactor p == 2 =
    if bfactor (getRight p) < 0
      then rotateLeft (setRight p (rotateRight (getRight p)))
      else rotateLeft p
  | bfactor p == -2 =
    if bfactor (getLeft p) > 0
      then rotateRight (setLeft p (rotateLeft (getLeft p)))
      else rotateRight p
  | otherwise = p

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch Meta {getSize = 1, getDepth = 1} Leaf x Leaf
tinsert x p@(Branch _ _ a _)
  | x == a = p
  | x < a = balance (setLeft p (tinsert x (getLeft p)))
  | otherwise = balance (setRight p (tinsert x (getRight p)))
