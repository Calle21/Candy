module Syn where

import qualified Data.ByteString.Char8 as C
import Text.Regex.PCRE ((=~))
import Ubi

 -- Punctuation

punctuation c = elem c ",;"

 -- Syn

floatSyn, hexSyn, intSyn, labSyn, nameSyn, octSyn :: C.ByteString -> Bool

floatSyn s = C.unpack s =~ "^[0-9]+\\.[0-9]+$"

hexSyn s = C.unpack s =~ "^0h-?[0-9a-fA-F]+$"

intSyn s = C.unpack s =~ "^-?[1-9][0-9]*$"

labSyn s = C.unpack s =~ "^[a-zA-Z_][a-zA-Z_0-9]*:$"

nameSyn s = let s' = C.unpack s
            in s' =~ "^[a-z0-9]*$" && any isLower s'

octSyn s = C.unpack s =~ "^0-?[0-7]+$"

 -- Token

tokChar = not . whiteSpace

 -- Whitespace

whiteSpace c = c == ' ' || c == '\t' || c == '\n'

whiteSpace' c = c == ' ' || c == '\t'
