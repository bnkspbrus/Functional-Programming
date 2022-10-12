module HW0.T5 where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns nat f a = f (nat f a)

nplus :: Nat a -> Nat a -> Nat a
nplus nat1 nat2 f = nat1 f . nat2 f
nmult :: Nat a -> Nat a -> Nat a
nmult nat1 nat2 f = nat1 (nat2 f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 f a = nz f a
nFromNatural n f a = f . nFromNatural (n - 1) f $ a

nToNum :: Num a => Nat a -> a
nToNum nat = nat (+ 1) 0
