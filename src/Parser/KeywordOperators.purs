module Tokenizer.KeywordOperators 
    {-( join
    )-} where

import Prelude
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as S
import Data.Array as A
import Data.List as L
import Control.Alt ((<|>))

import Tokenizer.Keywords as K
import Tokenizer.Tokens ( Parser, Token(..) )

    {-
join :: Parser 
join = do 
    join_ <- outerJoin <|> innerJoin <|> naturalJoin 
    pure join_
    
    where 
        outerJoin :: Parser
        outerJoin = do 
            direction_ <- C.option [] direction
            out <- K.outer
            join <- K.join
            pure $ direction_ <> out

            where direction = (K.left <|> K.right)

        innerJoin :: Parser 
        innerJoin = do 
            inner <- K.inner
            join <- K.join
            pure $ inner <> join

        naturalJoin :: Parser 
        naturalJoin = do 
            natural <- K.natural 
            join <- K.join 
            pure $ natural <> join

orderBy :: Parser 
orderBy = do 
    orderBy_ <- K.orderBy
    direction <- C.option [ Ascending ] (K.ascending <|> K.descending)
    pure $ orderBy_ <> direction

union :: Parser 
union = do 
    union_ <- K.union 
    duplicates <- C.option [ Distinct ] (K.all <|> K.distinct )
    pure $ union_ <> duplicates

intersect :: Parser
intersect = do 
    intersect_ <- K.intersect 
    duplicates <- C.option [ Distinct ] (K.all <|> K.distinct )
    pure $ intersect_ <> duplicates
-}