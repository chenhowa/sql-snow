module Tokenizer.Identifiers
    ( identifier

    ) where 

import Data.Identity
import Prelude

import Data.Array as A
import Data.Either as E
import Data.List as L
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Token as PT
import Tokenizer.Tokens (TokenParser, Token(..))
import Tokenizer.Utilities as U
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))




identifier :: TokenParser
identifier = do 
    first <- PT.letter
    remaining <- A.many $ C.try do 
        an <- C.optionMaybe $ C.try PT.alphaNum 
        res <- case an of 
            Just c -> pure c
            Nothing -> S.char '_'
        pure res    
    pure $ Identifier (fromCharArray $ A.concat [[first], remaining])

