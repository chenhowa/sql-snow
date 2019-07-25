module Parser.Clauses 
    ( select
    , where_
    ) where

import Prelude
import Data.List as L
import Data.Array as A
import Data.Identity
import Tokenizer.Tokens (Token)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Control.Alt ((<|>))


import Parser.Common (SyntaxArrayParser, ParseTree(..), Syntax(..), InputStream, Output, Parser)
import Parser.Syntax as Syntax


select :: Parser
select = do 
    sel <- Syntax.select
    expr <- selectExpression
    pure $
        Node sel $
            L.fromFoldable $ (Leaf <$> (expr))
    where 
        selectExpression :: SyntaxArrayParser
        selectExpression = do 
                C.try 
                    ( do 
                        wc <- Syntax.wildcard
                        pure [wc]
                    ) 
            <|> C.try selectColumns
        selectColumns :: SyntaxArrayParser 
        selectColumns = do 
            cols <- A.many $ C.try do 
                name <- Syntax.identifier
                comma <- Syntax.comma
                pure name
            final <- Syntax.identifier
            pure $ cols <> [final]

where_ :: Parser
where_ = do 
    wh <- Syntax.where_
    col <- Syntax.identifier
    op <- Syntax.operator
    con <- Syntax.constant
    pure $ 
        Node wh $ 
            L.fromFoldable $ (Leaf <$> [col, op, con])

