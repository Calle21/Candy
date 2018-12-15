module LEA.Scan where

scan :: [[Token]] -> Interface
scan lexed = loop [] [] 0 lexed
  where
  loop :: [(ByteString,Int)] -> [[Token]] -> Int -> [[Token]] -> Interface
  loop labels instructions _    []     = Interface labels (reverse instructions)
  loop labels instructions size (x:xs)
    | x `match` one isLabel >=> isEnd = loop ((tokenString x, size) : labels) instructions size xs
    | otherwise                       = let sizex = getSizeOfInstruction x
                                        in loop labels (x : instructions) (size + sizex) xs
    where
    getSizeOfInstruction :: [Token] -> Int
    getSizeOfInstruction xs
      | xs `match` one isGoto >=> one isLabel >=> isEnd = 1
      | xs `match` one isIf = let (s,xs') getSizeOfComparison
                              in if xs' `match` one isArrow >=> one isName >=> isEnd
                                 then s + 1
                                 else error "Error on if"
        where
        getSizeOfComparison xs = case one
