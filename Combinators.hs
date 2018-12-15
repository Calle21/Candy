module Combinators where

import Types
import Ubi

 -- Match

infix 4 `match`

match :: [Token] -> CombiMonad -> Bool
toks `match` m = isJust $ m toks

 -- Monads

isComparison :: CombiMonad
isComparison xs = 

isEnd :: CombiMonad
isEnd [] = Just []
isEnd _  = Nothing

notEnd :: CombiMonad
notEnd [] = Nothing
notEnd xs = Just xs

 -- Monad builders

one :: (Token -> Bool) -> CombiMonad
one p (x:xs) | p x = Just xs
one _ _ = Nothing

 -- Predicates

isAnd = is (Operator (C.pack "&"))

isCloseParen = is (Punct ')')

isComma = is (Punct ',')

isEqual = is (Comparison (C.pack "="))

isGoto = is (Reserved (C.pack "goto"))

isIf = is (Reserved (C.pack "if"))

isLabel (Label _) = True
isLabel _         = False

isMinus = is (Operator (C.pack "-"))

isNot = is (Operator (C.pack "!"))

isOpenParen = is (Punct '(')

isOr = is (Operator (C.pack "|"))

isOver = is (Operator (C.pack "/"))

isPlus = is (Operator (C.pack "+"))

isSemi = is (Punct ';')

isTimes = is (Operator (C.pack "*"))

isXor = is (Operator (C.pack "$"))

 -- Predicate builders

is :: Token -> Token -> Bool
is t0 t1 = t0 == t1
