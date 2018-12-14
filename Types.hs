module Types where

import qualified Data.ByteString.Char8 as C

type CombiMonad = [Token] -> Maybe [Token]

data CommandType = Op | Branch | Label'

data Token = FloatTok Float
           | IntTok Int
           | Label C.ByteString
           | Name C.ByteString
           | Punct Char
           | Reserved C.ByteString
           | Special C.ByteString
           deriving (Eq)

tokenFloat :: Token -> Float
tokenFloat (FloatTok f) = f

tokenInt :: Token -> Int
tokenInt (IntTok i) = i

tokenChar :: Token -> Char
tokenChar (Punct c) = c

tokenString :: Token -> C.ByteString
tokenString t = case t of
                  Label s   -> s
                  Name s    -> s
                  Special s -> s

tokenToString :: Token -> String
tokenToString tok = case tok of
                      FloatTok f -> show f
                      IntTok i   -> show i
                      Label s    -> C.unpack s ++ ":"
                      Name s     -> C.unpack s
                      Punct c    -> [c]
                      Special s  -> C.unpack s
