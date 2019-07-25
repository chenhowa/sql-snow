module Tokenizer.Operators
    ( plus
    , minus 
    , multiply 
    , floatDivide 
    , modulo
    , equals
    , notEquals
    , not 
    , and 
    , or 
    , lt 
    , lte
    , gt 
    , gte 
    ) where 

import Prelude
import Text.Parsing.Parser.String as S
import Tokenizer.Tokens ( TokenParser, Token(..) )
import Tokenizer.Utilities as U

plus :: TokenParser 
plus = do 
    _ <- S.string "+"
    pure $ Plus

minus :: TokenParser 
minus = do 
    _ <- S.string "-"
    pure $  Minus 

multiply :: TokenParser 
multiply = do 
    _ <- S.string "*"
    pure $  Asterisk 

floatDivide :: TokenParser 
floatDivide = do 
    _ <- S.string "/"
    pure $  FloatDivide 

modulo :: TokenParser 
modulo = do 
    _ <- S.string "%"
    pure $  Modulo 

equals :: TokenParser
equals = do 
    _ <- S.string "="
    pure $ Equals

notEquals :: TokenParser 
notEquals = do 
    _ <- S.string "!="
    pure $ NotEquals

not :: TokenParser
not = do 
    _ <- U.caseInsensitive "not"
    pure $ Not

and :: TokenParser 
and = do 
    _ <- U.caseInsensitive "and"
    pure And

or :: TokenParser
or = do 
    _ <- U.caseInsensitive "or"
    pure Or

lt :: TokenParser
lt = do 
    _ <- S.string "<"
    pure LT 

lte :: TokenParser 
lte = do 
    _ <- S.string "<="
    pure LTE

gt :: TokenParser 
gt = do 
    _ <- S.string ">"
    pure GT 

gte :: TokenParser 
gte = do 
    _ <- S.string ">="
    pure GTE
