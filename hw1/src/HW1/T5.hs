module HW1.T5 where

import qualified Data.List.NonEmpty as N
--import Prelude hiding (head, tail)

splitOn :: Eq a => a -> [a] -> N.NonEmpty [a]
--splitOn _ [] = [] N.:| []
--splitOn d (x : xs) = if x == d then [] N.<| splitOn d xs else let r = splitOn d xs in (x : N.head r) N.:| N.tail r
splitOn d = foldr (\x r -> if x == d then [] N.<| r else (x : N.head r) N.:| N.tail r) ([] N.:| [])

joinWith :: a -> N.NonEmpty [a] -> [a]
joinWith d = tail . concatMap (d :)
