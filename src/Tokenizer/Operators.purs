module Tokenizer.Operators 
    ( join
    ) where

import Prelude
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as S
import Data.Array as A
import Data.List as L
import Control.Alt ((<|>))

import Tokenizer.Keywords as K
import Tokenizer.Keywords ( Parser)

      
join :: Parser 
join = do 
    join_ <- outerJoin <|> innerJoin <|> naturalJoin 
    pure join_

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