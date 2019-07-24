module Tokenizer.Tokens
    ( Parser
    , TokenParser
    , InputStream 
    , TokenStream
    , Token(..)
    ) where 

import Prelude
import Text.Parsing.Parser as P
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)

type Parser = P.Parser InputStream TokenStream
type TokenParser = P.Parser InputStream Token
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
    | As
    | Identifier String
    | Constant String
    | RightParen
    | LeftParen
    | Comma
    | LineComment String
    | BlockComment String
    | WhiteSpace

derive instance genericToken :: Generic Token _
instance showToken :: Show Token where 
    show = genericShow
instance eqToken :: Eq Token where 
    eq = genericEq