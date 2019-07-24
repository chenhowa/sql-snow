module Tokenizer 
    ( tokenize
    , runToken
    ) where 


import Data.Identity
import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List as L
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S
import Tokenizer.Keywords as K
import Tokenizer.Operators as O
import Tokenizer.Tokens (TokenParser, InputStream, TokenStream, Token(..), Parser)
import Tokenizer.Utilities as U
import Data.Maybe(Maybe(..))


tokenize :: InputStream -> Either P.ParseError TokenStream
tokenize input = 
    let Identity result = P.runParserT input tokens
    in result

runToken :: String -> Either P.ParseError Token
runToken str = 
    let Identity result = P.runParserT str token
    in result

tokens :: Parser 
tokens = do 
    S.skipSpaces
    ts <- (A.many (try do
        t <- token
        U.skipSpaces   -- discards required whitespace between tokens
        pure t))
    t <- C.optionMaybe (try token)  -- We need to use optionMaybe with try, because if the parse fails but consumes no input, 
    case t of                       -- what result is extracted into t? Nothing can be. Therefore the try basically is ignored,
        Nothing -> do               -- and the parse is regarded as having failed.
            S.eof
            pure $ A.concat [ts]
        Just last -> do 
            S.eof
            pure $ A.concat [ts, [last]]

token :: TokenParser
token = 
    let parsers = A.concat
            [
                [ K.distinct 
                , K.limit 
                , K.all 
                ]
            , joinParsers
            , operatorParsers
            , orderParsers 
            , groupParsers
            , setOpParsers
            , selectParsers
            ]
    in foldr (<|>) K.in_ (try <$> parsers)

    where 
        joinParsers :: Array TokenParser 
        joinParsers = 
                    [ K.right 
                    , K.inner 
                    , K.outer 
                    , K.natural
                    , K.join 
                    , K.on
                    , K.left
                    ]

        operatorParsers :: Array TokenParser 
        operatorParsers = 
                    [ O.minus 
                    , O.multiply 
                    , O.floatDivide 
                    , O.modulo
                    , O.plus
                    ]

        orderParsers :: Array TokenParser 
        orderParsers = 
                    [ K.ascending 
                    , K.descending 
                    , K.orderBy
                    ]

        groupParsers :: Array TokenParser 
        groupParsers = 
                    [ K.having
                    , K.groupBy
                    ]

        setOpParsers :: Array TokenParser
        setOpParsers = 
                    [ K.intersect 
                    , K.union
                    ]

        selectParsers :: Array TokenParser 
        selectParsers = 
                    [ K.from 
                    , K.where_ 
                    , K.select
                    ]
    