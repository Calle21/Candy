module Util where

import qualified Data.ByteString.Char8 as C
import Ubi

 -- elemOn

elemOn :: Eq b => (a -> b) -> b -> [a] -> Bool
elemOn fn elt (x:xs) = fn x == elt || elemOn fn elt xs
elemOn _  _   _      = False

 -- inRange

inRange :: Ord a => a -> (a,a) -> Bool
inRange a (lo,hi) = a >= lo && a <= hi

 -- noNull

noNull :: [[a]] -> [[a]]
noNull [] = []
noNull (x:xs) | null x    = noNull xs
              | otherwise = x : noNull xs

 -- or

infixr 9 |||

(|||) -> (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f0 ||| f1 = \x -> f0 x || f1 x

 -- readint

readint :: Int -> C.ByteString -> Int
readint base s = let (sign, i) | C.head s == '-' = (-1, 1)
                               | otherwise       = (1, 0)
                 in sign * loop s 0 i
  where
  loop :: C.ByteString -> Int -> Int -> Int
  loop s acc i | i == C.length s = acc
               | otherwise       = loop s (acc * base + digitToInt (s `C.index` i)) (i + 1)

 -- safeTail

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

 -- Tuple

1of3 :: (a,b,c) -> a
2of3 :: (a,b,c) -> b
3of3 :: (a,b,c) -> c
1of3 (a,_,_) = a
2of3 (_,b,_) = b
3of3 (_,_,c) = c
