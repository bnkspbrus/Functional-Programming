module HW0.T4 where

import Data.Function (fix)
import Numeric.Natural (Natural)


repeat' :: a -> [a]             -- behaves like Data.List.repeat
repeat' x = fix (x:)
map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
map' = fix (\rec f arr -> if null arr then [] else f (head arr) : rec f (tail arr))
fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib = fix f
  where 
    f _ 0 = 0
    f _  1 = 1
    f rec n = rec (n - 1) + rec (n - 2)
fac :: Natural -> Natural       -- computes the factorial
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n-1))