module HW0.T2 where

import Data.Void


type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg a = f
  where f g = g a
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f = g
  where g a = f (doubleNeg a)