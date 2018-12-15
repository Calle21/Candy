module Ubi (
          module Control.Monad
        , module Data.ByteString.Char8
        , elem'
        , prefix
        , module Data.Char
        , module Data.List
        , module Data.Maybe
          ) where

import Control.Monad (
                      (>=>)
                      )

import qualified Data.ByteString.Char8 as C

elem'  = C.elem
prefix = C.isPrefixOf

import Data.ByteString.Char8 (
                              ByteString
                            , pack
                            , unpack
                              )

import Data.Char (
                  digitToInt
                , isLower
                  )

import Data.List (
                  elemIndex
                , intercalate
                  )

import Data.Maybe (
                   isJust
                   )
