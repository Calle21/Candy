module Lex where

import qualified Data.ByteString.Char8 as C
import Syn
import Types
import Util

lex' :: C.ByteString -> [[Token]]
lex' s = noNull $ loop [] s
  where
  loop :: [Token] -> C.ByteString -> [[Token]]
  loop acc s
    | C.null s  = [reverse acc]
    | otherwise = case C.head s of
                    '\n' -> reverse acc : loop [] (C.dropWhile whiteSpace s)
                    '#'  -> loop acc (C.dropWhile (/='\n') s)
                    c    -> if whiteSpace c
                            then loop acc (C.dropWhile whiteSpace' s)
                            else if punctuation c then loop (Punct c : acc) (C.tail s)
                                 else let (toks,s') = C.span tokChar s
                                          tok | floatSyn toks = FloatTok $ read $ C.unpack toks
                                              | hexSyn toks   = IntTok $ readint 16 (C.drop 2 toks)
                                              | intSyn toks   = IntTok $ readint 10 toks
                                              | labSyn toks   = Label (C.init toks)
                                              | nameSyn toks  = Name toks
                                              | octSyn toks   = IntTok $ readint 8 (C.tail toks)
                                              | otherwise     = Special toks
                                      in loop (tok : acc) s'
