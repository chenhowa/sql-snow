module Tokenizer.Operators
    ( plus
    , minus 
    , multiply 
    , floatDivide 
    , modulo

    ) where 

import Prelude
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as S
import Data.Array as A
import Data.List as L
import Control.Alt ((<|>))

import Tokenizer.Tokens ( TokenParser, Token(..) )

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
    pure $  Multiply 

floatDivide :: TokenParser 
floatDivide = do 
    _ <- S.string "/"
    pure $  FloatDivide 

modulo :: TokenParser 
modulo = do 
    _ <- S.string "%"
    pure $  Modulo 