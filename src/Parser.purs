module Parser 
    ( parser

    ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List as L
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

import Parser.Common (ParseTree(..), Syntax(..), InputStream, Output, Parser)

parser :: Parser
parser = do 
    pure $ Leaf Select