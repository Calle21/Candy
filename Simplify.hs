module Simplify where

import Ubi

simplify :: [[Token]] -> (Variables, [[Token]])
simplify (y:ys) = let vars = getVars y
                  in loop vars [] ys
  where
  getVars :: [Token] -> Variables
  getVars xs = let (ret,xs')   = get [] xs
                   (arg,xs'')  = get [] xs'
                   (var,xs''') = get [] (xs'' ++ Punct ';')
               in if null xs'''
                  then (var,arg,ret)
                  else error "Bad declaration (expected null)"
    where
    get :: [Token] -> [Token] -> ([(ByteString,Type)],[Token])
    get acc (x:xs) | isName x  = let (type', xs') | xs `match` one isOpenParen
                                                           >=> one isName
                                                           >=> one isCloseParen = (getType (xs !! 1), drop 3 xs)
                                                  | xs `match` one isComma      = (Signed, tail xs)
                                                  | otherwise                   = error "Bad declaration"
                                 in get ((x, type') : acc) x'
                   | isSemi x  = (acc,xs)
                   | otherwise = error "Bad declaration (expected name or semi)"
      where
      getType :: Token -> Type
      getType (Name s) | s `prefix` pack "signed"   = Signed
                       | s `prefix` pack "unsigned" = Unsigned
                       | s `prefix` pack "float"    = Float'
      getType _ = error "Bad type prefix"
    get _ _ = error "Bad declaration (didn't expect null)"
  loop :: Variables -> [[Token]] -> (Variables, [[Token]])
  loop vars acc [] = (vars, reverse acc)
  loop vars acc (y:ys)
    | y `match` one isLabel >=> isEnd               = loop vars (y : acc) xs
    | y `match` one isGoto >=> one isName >=> isEnd = loop vars (y : acc) xs
    | y `match` one isIf = let (e0,xs) = break isComparison y
                               dep     = exprDepth vars e0
    | y `match` one (isVar vars) >=> one isEqual =
    | otherwise = error "Error in simplification"
    where
    isVar :: Variables -> Token -> Bool
    isVar (a,b,c) t | isName t = let s = tokenString t
                                 in s `elem'` a || s `elem'` b || s `elem'` c
      where elem' = elemOn fst
    isVar _ _ = False
    exprDepth :: Variables -> [Token] -> ExprType
    exprDepth vars xs = case one (isFloat ||| isInt ||| isVar vars) xs of
                          Just xs' = if null xs' then 1
                                     else if isOperator 
                          Nothing  = error "Bad expression"
