module Parser.Common 
    ( ParseTree(..)
    , Syntax(..)
    , InputStream 
    , Output
    , Parser
    ) where 

import Prelude
import Data.List as L
import Data.Array as A
import Data.Identity
import Tokenizer.Tokens (Token)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

data ParseTree a
    = Leaf a
    | Node a (L.List (ParseTree a))

data Syntax
    = Select 
    | Where

type InputStream = Array Token
type Output = ParseTree Syntax
type Parser = P.ParserT InputStream Identity Output