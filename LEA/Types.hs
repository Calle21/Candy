module LEA.Types where

import Ubi (ByteString)

data Interface = Interface {labels       :: [(ByteString,Int)]
                          , instructions :: [[Token]]}

data LEAInstruction = LEALabel ByteString | LEAOperation ByteString [Token]

data Operation = Operation {opcode :: ByteString
                          , args   :: [Int]}
