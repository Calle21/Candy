module Main (main) where

import qualified Data.ByteString.Char8 as C
import Lex
import System.Environment (getArgs)
import Types

main = do [path] <- getArgs
          s <- C.readFile path
          let tokens = lex' s
          putLex tokens

putLex :: [[Token]] -> IO ()
putLex (x:xs) = do putToks x
                   loop xs
  where
  loop :: [[Token]] -> IO ()
  loop []     = return ()
  loop (x:xs) = do case head x of
                     Label _ -> return ()
                     _       -> putChar '\t'
                   putToks x
                   loop xs
  putToks :: [Token] -> IO ()
  putToks []     = putChar '\n'
  putToks (x:xs) = do putStr (tokenToString x)
                      putChar ' '
                      putToks xs
