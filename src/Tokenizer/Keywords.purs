module Tokenizer.Keywords 
    ( select 
    , from 
    , where_ 
    , groupBy 
    , having 
    , in_
    , distinct
    , limit 
    , orderBy
    , ascending 
    , descending
    , Parser
    , InputStream 
    , TokenStream
    , Token(..)
    ) where 

import Prelude
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as S
import Data.Array as A
import Data.List as L

type Parser = P.Parser InputStream TokenStream
type InputStream = String
type TokenStream = Array Token

data Token
    = Select
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

select :: Parser
select = do 
    _ <- S.string "SELECT"
    pure [Select]

from :: Parser
from = do
    _ <- S.string "FROM"
    pure [ From ]

where_ :: Parser 
where_ = do
    _ <- S.string "WHERE"
    pure [ Where ]

groupBy :: Parser 
groupBy = do 
    _ <- S.string "GROUP"
    S.skipSpaces
    _ <- S.string "BY"
    pure [ GroupBy ]

having :: Parser
having = do 
    _ <- S.string "HAVING"
    pure [ Having ]

in_ :: Parser
in_ = do 
    _ <- S.string "IN"
    pure [ In ]

distinct :: Parser
distinct = do 
    _ <- S.string "DISTINCT"
    pure [ Distinct ]

limit :: Parser
limit = do 
    _ <- S.string "LIMIT"
    pure [ Limit ]

orderBy :: Parser
orderBy = do 
    _ <- S.string "ORDER"
    S.skipSpaces
    _ <- S.string "BY"
    pure [ OrderBy ]

ascending :: Parser 
ascending = do 
    _ <- S.string "ASC"
    pure [ Ascending ]

descending :: Parser
descending = do 
    _ <- S.string "DESC"
    pure [ Descending ]

union :: Parser 
union = do
    _ <- S.string "UNION"
    pure [ Union ]

intersect :: Parser 
intersect = do 
    _ <- S.string "INTERSECT"
    pure [ Intersect ]

all :: Parser 
all = do 
    _ <- S.string "ALL"
    pure [ All ]

left :: Parser 
left = do 
    _ <- S.string "LEFT"
    pure [ Left ]

right :: Parser 
right = do 
    _ <- S.string "RIGHT"
    pure [ Right ]

inner :: Parser 
inner = do 
    _ <- S.string "INNER"
    pure [ Inner ]

outer :: Parser 
outer = do 
    _ <- S.string "OUTER"
    pure [ Outer ] 

natural :: Parser 
natural = do 
    _ <- S.string "NATURAL"
    pure [ Natural ]