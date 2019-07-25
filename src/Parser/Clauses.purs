module Parser.Clauses 
    ( select

    ) where

import Prelude
import Data.List as L
import Data.Array as A
import Data.Identity
import Tokenizer.Tokens (Token)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C

import Parser.Common (ParseTree(..), Syntax(..), InputStream, Output, Parser)


select :: Parser
select = do 
    pure $ Leaf Select
    