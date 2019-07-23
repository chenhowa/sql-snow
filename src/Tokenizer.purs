module Tokenizer 
    ( tokenize
    ) where 


import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.List as L
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S
import Tokenizer.Keywords as K
import Tokenizer.Operators as O
import Tokenizer.Tokens (TokenParser, InputStream, TokenStream, Token(..), Parser)
import Data.Either(Either(..))
import Data.Identity



tokenize :: InputStream -> Either P.ParseError TokenStream
tokenize input = 
    let Identity result = P.runParserT input tokens
    in result

tokens :: Parser 
tokens = do 
    S.skipSpaces
    ts <- A.many do
        t <- token
        S.skipSpaces
        pure t
    pure $ ts

token :: TokenParser
token = 
        K.in_ 
    <|> K.distinct 
    <|> K.limit 
    <|> K.all 
    <|> joinTokens 
    <|> operatorTokens 
    <|> orderTokens
    <|> groupTokens
    <|> setOpTokens
    <|> selectTokens

    where 
        joinTokens :: TokenParser 
        joinTokens =
                K.left 
            <|> K.right 
            <|> K.inner 
            <|> K.outer 
            <|> K.natural
            <|> K.join 
            <|> K.on

        operatorTokens :: TokenParser 
        operatorTokens = 
                O.plus 
            <|> O.minus 
            <|> O.multiply 
            <|> O.floatDivide 
            <|> O.modulo

        orderTokens :: TokenParser 
        orderTokens = 
                K.orderBy 
            <|> K.ascending 
            <|> K.descending 

        groupTokens :: TokenParser 
        groupTokens = 
                K.groupBy 
            <|> K.having

        setOpTokens :: TokenParser
        setOpTokens = 
                K.union 
            <|> K.intersect 

        selectTokens :: TokenParser 
        selectTokens = 
                K.select
            <|> K.from 
            <|> K.where_ 