module Util where

import qualified Data.ByteString.Char8 as C
import Ubi

 -- inRange

inRange :: Ord a => a -> (a,a) -> Bool
inRange a (lo,hi) = a >= lo && a <= hi

 -- noNull

noNull :: [[a]] -> [[a]]
noNull [] = []
noNull (x:xs) | null x    = noNull xs
              | otherwise = x : noNull xs

 -- readint

readint :: Int -> C.ByteString -> Int
readint base s = let (sign, i) | C.head s == '-' = (-1, 1)
                               | otherwise       = (1, 0)
                 in sign * loop s 0 i
  where
  loop :: C.ByteString -> Int -> Int -> Int
  loop s acc i | i == C.length s = acc
               | otherwise       = loop s (acc * base + digitToInt (s `C.index` i)) (i + 1)


