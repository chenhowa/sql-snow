module Tokenizer.Operators
    ( plus

    ) where 

import Prelude
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as S
import Data.Array as A
import Data.List as L
import Control.Alt ((<|>))

import Tokenizer.Tokens ( Parser, Token(..) )

plus :: Parser 
plus = do 
    _ <- S.string "+"
    pure $ [Plus]

minus :: Parser 
minus = do 
    _ <- S.string "-"
    pure $ [ Minus ]

multiply :: Parser 
multiply = do 
    _ <- S.string "*"
    pure $ [ Multiply ]

floatDivide :: Parser 
floatDivide = do 
    _ <- S.string "/"
    pure $ [ FloatDivide ]

modulo :: Parser 
modulo = do 
    _ <- S.string "%"
    pure $ [ Modulo ]