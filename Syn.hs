module Syn where

import Text.Regex.PCRE ((=~))
import Ubi

 -- Comparisons

comparisons = map pack ["=","<",">","<=",">="]

 -- Operators

operators = map pack ["+","-","*","/","&","|","$","^","!"]

 -- Parse

initSyn :: [Token] -> Bool
initSyn xs = all (\t -> is (Punct 

 -- Punctuation

punctuation c = elem c ",;()"

 -- Reserved

reservedNames = map pack ["goto", "if", "return"]

reserved s = s `elem` reservedNames

 -- Syn

arrowSyn, comparisonSyn, floatSyn, hexSyn, intSyn, labSyn, nameSyn, octSyn, opSyn :: ByteString -> Bool

arrowSyn s = s `elem` operators

comparisonSyn s = s `elem` comparisons

floatSyn s = unpack s =~ "^[0-9]+\\.[0-9]+$"

hexSyn s = unpack s =~ "^0h-?[0-9a-fA-F]+$"

intSyn s = unpack s =~ "^-?[1-9][0-9]*$"

labSyn s = unpack s =~ "^[a-zA-Z_][a-zA-Z_0-9]*:$"

nameSyn s = let s' = unpack s
            in s' =~ "^[a-z0-9]*$" && any isLower s'

octSyn s = unpack s =~ "^0-?[0-7]+$"

opSyn s = s `elem` operators

 -- Token

tokChar = not . whiteSpace

 -- Whitespace

whiteSpace c = c == ' ' || c == '\t' || c == '\n'

whiteSpace' c = c == ' ' || c == '\t'
