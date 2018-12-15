module LEA.Parse where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi

parse :: [[Token]] -> [LEAInstruction]
parse (x:xs) | initSyn x = parseBody (getVars x) xs
             | otherwise = error "Expected var declaration at beginning"
  where
  parseBody :: (C.ByteString -> Int) -> [Token] -> ???
  parseBody position toks = case cmdType (head toks) of
                              Branch ->
                              Label' ->
                              Op     ->

getVars :: [Token] -> C.ByteString -> Maybe Int
getVars xs = let (rets,xs')  = get [] xs
                 (args,xs'') = get [] xs'
                 (vars,_)    = get [] xs''
             in succ . flip elemIndex (vars ++ args ++ rets)
  where
  get :: [Token] -> [Token] -> ([C.ByteString],[Token])
  get acc (x:xs) = let s = tokenString x
                   if null xs || is (Punct ';') (head xs)
                   then (acc,tail xs)
                   else get (s : acc) (tail xs)

cmdType :: [Token] -> CommandType
cmdType xs | xs `match` (one isGoto >=> one isName >=> isEnd)  = Branch
           | xs `match` (one isLabel >=> isEnd)                = Label'
           | xs `match` one isName >=> one isEqual >=> notEnd) = Op
           | otherwise                                      = error "Syntax error"
