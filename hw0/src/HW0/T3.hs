module HW0.T3 where

s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

k :: a -> b -> a
k x y = x

i :: a -> a
i = s k k

ss :: ((a -> b -> c) -> (a -> b)) -> (a -> b -> c) -> (a -> c)
ss = s s

ki :: (a -> b -> c) -> a -> a
ki = k i

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

contract :: (a -> a -> b) -> (a -> b)
contract = s s (k i)

permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k s) (s (s (k s) (s (k k) (k s))) (s (k k) (s k k)))) (k k)

