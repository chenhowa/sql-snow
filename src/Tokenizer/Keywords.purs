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
    , union 
    , intersect 
    , all 
    , left 
    , right 
    , inner 
    , outer 
    , natural
    , join 
    , on
    , runKeyword
    ) where 

import Prelude

import Data.Array as A
import Data.Either as E
import Data.List as L
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S
import Tokenizer.Tokens (TokenParser, Token(..))
import Tokenizer.Utilities as U
import Data.Identity


runKeyword :: String -> TokenParser -> E.Either P.ParseError Token
runKeyword str parser = 
    let Identity r = P.runParserT str parser
    in r

select :: TokenParser
select = do
    _ <- U.caseInsensitive "SELECT"
    pure Select

from :: TokenParser
from = do
    _ <- U.caseInsensitive "FROM"
    pure From

where_ :: TokenParser 
where_ = do
    _ <- U.caseInsensitive "WHERE"
    pure Where

groupBy :: TokenParser 
groupBy = do 
    _ <- U.caseInsensitive "GROUP"
    S.skipSpaces
    _ <- U.caseInsensitive "BY"
    pure GroupBy

having :: TokenParser
having = do 
    _ <- U.caseInsensitive "HAVING"
    pure Having

in_ :: TokenParser
in_ = do 
    _ <- U.caseInsensitive "IN"
    pure In

distinct :: TokenParser
distinct = do 
    _ <- U.caseInsensitive "DISTINCT"
    pure Distinct

limit :: TokenParser
limit = do 
    _ <- U.caseInsensitive "LIMIT"
    pure Limit

orderBy :: TokenParser
orderBy = do 
    _ <- U.caseInsensitive "ORDER"
    S.skipSpaces
    _ <- U.caseInsensitive "BY"
    pure OrderBy

ascending :: TokenParser 
ascending = do 
    _ <- U.caseInsensitive "ASC"
    pure Ascending

descending :: TokenParser
descending = do 
    _ <- U.caseInsensitive "DESC"
    pure Descending

union :: TokenParser 
union = do
    _ <- U.caseInsensitive "UNION"
    pure Union

intersect :: TokenParser 
intersect = do 
    _ <- U.caseInsensitive "INTERSECT"
    pure Intersect

all :: TokenParser 
all = do 
    _ <- U.caseInsensitive "ALL"
    pure All

left :: TokenParser 
left = do 
    _ <- U.caseInsensitive "LEFT"
    pure Left

right :: TokenParser 
right = do 
    _ <- U.caseInsensitive "RIGHT"
    pure Right

inner :: TokenParser 
inner = do 
    _ <- U.caseInsensitive "INNER"
    pure Inner

outer :: TokenParser 
outer = do 
    _ <- U.caseInsensitive "OUTER"
    pure Outer

natural :: TokenParser 
natural = do 
    _ <- U.caseInsensitive "NATURAL"
    pure Natural

join :: TokenParser 
join = do 
    _ <- U.caseInsensitive "JOIN"
    pure Join

on :: TokenParser 
on = do 
    _ <- U.caseInsensitive "ON"
    pure On

as :: TokenParser 
as = do 
    _ <- U.caseInsensitive "AS"
    pure As