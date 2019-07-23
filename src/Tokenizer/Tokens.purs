module Tokenizer.Tokens
    ( Parser
    , InputStream 
    , TokenStream
    , Token(..)
    ) where 

import Text.Parsing.Parser as P


type Parser = P.Parser InputStream TokenStream
type InputStream = String
type TokenStream = Array Token

data Token
    = Select -- Keyword tokens
    | From
    | Where
    | GroupBy
    | Having
    | In 
    | Distinct 
    | Limit 
    | OrderBy
    | Ascending
    | Descending
    | Union
    | Intersect
    | All
    | Left 
    | Right 
    | Inner
    | Outer 
    | Natural
    | Join
    | On
    | Plus -- Arithmetic tokens
    | Minus
    | Multiply
    | FloatDivide
    | Modulo