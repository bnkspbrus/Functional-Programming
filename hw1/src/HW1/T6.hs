module HW1.T6
  ( mcat,
    epart,
  )
where

import Data.Foldable

--import Data.Monoid

--import Data.Maybe (fromJust)

mcat :: Monoid a => [Maybe a] -> a
mcat = fold . fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap f
  where
    f (Left a)  = (a, mempty)
    f (Right b) = (mempty, b)
