module Types where

import Ubi

type CombiMonad = [Token] -> Maybe [Token]

data CommandType = Op | Branch | Label'

data ExprType = Complex | Simple

data Token = Arrow
           | Call ByteString [Token]
           | FloatTok Float
           | IntTok Int
           | Label ByteString
           | Name ByteString
           | Operator ByteString
           | Punct Char
           | Reserved ByteString
           | Special ByteString
           deriving (Eq)

tokenFloat :: Token -> Float
tokenFloat (FloatTok f) = f

tokenInt :: Token -> Int
tokenInt (IntTok i) = i

tokenChar :: Token -> Char
tokenChar (Punct c) = c

tokenString :: Token -> ByteString
tokenString t = case t of
                  Label s   -> s
                  Name s    -> s
                  Special s -> s

tokenToString :: Token -> String
tokenToString tok = case tok of
                      Call s xs  -> unpack s ++ " (" ++ intercalate ", " (map tokenToString xs) ++ ")"
                      FloatTok f -> show f
                      IntTok i   -> show i
                      Label s    -> unpack s ++ ":"
                      Name s     -> unpack s
                      Punct c    -> [c]
                      Special s  -> unpack s

data Type = Signed | Unsigned | Float'

type Variables = ([(ByteString,Type)],[(ByteString,Type)],[(ByteString,Type)])
