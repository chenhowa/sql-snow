module Tokenizer 
    ( tokenize
    ) where 


import Prelude
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as S
import Data.Array as A
import Data.List as L

import Tokenizer.Keywords as Keywords
import Tokenizer.Tokens (InputStream, TokenStream, Token(..), Parser)




tokenize :: InputStream -> TokenStream
tokenize input = []


selectQuery :: Parser
selectQuery = do
    select <- Keywords.select
    distinct <- Keywords.distinct
    S.skipSpaces
    from <- Keywords.from
    S.skipSpaces
    where_ <- Keywords.where_
    in_ <- Keywords.in_
    groupBy <- Keywords.groupBy
    having <- Keywords.having
    limit <- Keywords.limit
    pure [ In ]
