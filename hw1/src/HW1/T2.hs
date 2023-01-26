module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    ncmp,
    nFromNatural,
    nToNum,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)

data N = Z | S N deriving (Show)

nplus :: N -> N -> N -- addition
nplus n Z       = n
nplus n1 (S n2) = nplus (S n1) n2

nmult :: N -> N -> N -- multiplication
nmult _ Z       = Z
nmult n1 (S n2) = nplus n1 (nmult n1 n2)

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub Z Z           = Just Z
nsub Z _           = Nothing
nsub n Z           = Just n
nsub (S n1) (S n2) = nsub n1 n2

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp n1 n2 =
  let n3 = nsub n1 n2
   in case n3 of
        Nothing -> LT
        Just Z  -> EQ
        _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + nToNum n

nEven, nOdd :: N -> Bool -- parity checking
nEven Z     = True
nEven (S n) = not (nEven n)
nOdd = not . nEven

ndiv :: N -> N -> N -- integer division
ndiv _ Z   = error "division by zero"
ndiv n1 n2 = let n3 = nsub n1 n2 in maybe Z (S . (`ndiv` n2)) n3

nmod :: N -> N -> N -- modulo operation
nmod n1 n2 = let n3 = ndiv n1 n2 in fromMaybe Z (nsub n1 (nmult n2 n3))
