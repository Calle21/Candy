module Types where

import qualified Data.ByteString.Char8 as C

data Token = FloatTok Float
           | IntTok Int
           | Label C.ByteString
           | Name C.ByteString
           | Punct Char
           | Special C.ByteString

tokenToString :: Token -> String
tokenToString tok = case tok of
                      FloatTok f -> show f
                      IntTok i   -> show i
                      Label s    -> C.unpack s ++ ":"
                      Name s     -> C.unpack s
                      Punct c    -> [c]
                      Special s  -> C.unpack s
