module Combinators where

import Types
import Ubi

 -- Match

infix 4 `match`

match :: [Token] -> CombiMonad -> Bool
toks `match` m = isJust $ m toks

 -- Monads

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

isAnd = is (Special (C.pack "&"))

isComma = is (Punct ',')

isEqual = is (Special (C.pack "="))

isGoto = is (Reserved (C.pack "goto"))

isIf = is (Reserved (C.pack "if"))

isLabel (Label _) = True
isLabel _         = False

isMinus = is (Special (C.pack "-"))

isNot = is (Special (C.pack "!"))

isOr = is (Special (C.pack "|"))

isOver = is (Special (C.pack "/"))

isPlus = is (Special (C.pack "+"))

isSemi = is (Punct ';')

isTimes = is (Special "*")

isXor = is (Special (C.pack "$"))

 -- Predicate builders

is :: Token -> Token -> Bool
is t0 t1 = t0 == t1
