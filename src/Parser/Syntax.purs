module Parser.Syntax 
    ( select
    , identifier 
    , comma
    , wildcard
    , where_
    , operator
    , constant
    ) where

import Data.Identity
import Prelude

import Data.Array as A
import Data.List as L
import Parser.Common (ParseTree(..), Syntax(..), InputStream, Output, Parser, SyntaxParser)
import Parser.Parsing as Parsing
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Tokenizer.Tokens as T
import Data.Foldable (foldr)
import Control.Alt ((<|>))


select :: SyntaxParser
select = do 
    _ <- Parsing.token T.Select
    pure Select

identifier :: SyntaxParser
identifier = do 
    val <- Parsing.satisfy (\t -> case t of 
                            T.Identifier x -> true
                            _ -> false)
    case val of 
        T.Identifier x -> pure $ Identifier x
        _ -> P.fail "Identifier found, then not found"

comma :: SyntaxParser
comma = do 
    _ <- Parsing.token T.Comma
    pure Comma

wildcard :: SyntaxParser
wildcard = do 
    _ <- Parsing.token T.Asterisk
    pure Wildcard

where_ :: SyntaxParser
where_ = do 
    _ <- Parsing.token T.Where
    pure Where

plus :: SyntaxParser 
plus = do 
    _ <- Parsing.token T.Plus
    pure $ Plus

minus :: SyntaxParser 
minus = do 
    _ <- Parsing.token T.Minus
    pure $  Minus 

multiply :: SyntaxParser 
multiply = do 
    _ <- Parsing.token T.Asterisk
    pure $  Multiply 

floatDivide :: SyntaxParser 
floatDivide = do 
    _ <- Parsing.token T.FloatDivide
    pure $  FloatDivide 

modulo :: SyntaxParser 
modulo = do 
    _ <- Parsing.token T.Modulo
    pure $  Modulo 

equals :: SyntaxParser
equals = do 
    _ <- Parsing.token T.Equals
    pure $ Equals

notEquals :: SyntaxParser 
notEquals = do 
    _ <- Parsing.token T.NotEquals
    pure $ NotEquals

not :: SyntaxParser
not = do 
    _ <- Parsing.token T.Not
    pure $ Not

and :: SyntaxParser 
and = do 
    _ <- Parsing.token T.And
    pure And

or :: SyntaxParser
or = do 
    _ <- Parsing.token T.Or
    pure Or

lt :: SyntaxParser
lt = do 
    _ <- Parsing.token T.LT
    pure LT 

lte :: SyntaxParser 
lte = do 
    _ <- Parsing.token T.LTE
    pure LTE

gt :: SyntaxParser 
gt = do 
    _ <- Parsing.token T.GT
    pure GT 

gte :: SyntaxParser 
gte = do 
    _ <- Parsing.token T.GTE
    pure GTE

operator :: SyntaxParser
operator =
    let operators = 
            [ minus 
            , multiply
            , floatDivide
            , modulo
            , plus 
            , equals 
            , notEquals
            , not
            , and  
            , or 
            , gte
            , lte 
            , lt 
            ]
        parsers = (C.try ) <$> operators 
    in foldr (<|>) (C.try gt) parsers

constant :: SyntaxParser
constant = do
    token <- Parsing.satisfy \t -> case t of 
            T.Constant val -> true 
            _ -> false
    case token of 
        T.Constant val -> pure $ Constant val
        _ -> P.fail "Constant, then no constant"