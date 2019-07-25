module Parser.Syntax 
    ( select
    , identifier 
    , comma
    , wildcard
    ) where

import Prelude
import Data.List as L
import Data.Array as A
import Data.Identity
import Tokenizer.Tokens as T
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Parser.Common (ParseTree(..), Syntax(..), InputStream, Output, Parser, SyntaxParser)
import Parser.Parsing as Parsing

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

multiply :: SyntaxParser
multiply = do 
    _ <- Parsing.token T.Asterisk
    pure Multiply