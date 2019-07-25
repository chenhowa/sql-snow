module Parser.Syntax where 

import Prelude
import Data.List as L
import Data.Identity
import Tokenizer.Tokens (Token)
import Text.Parsing.Parser as P
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)


data Syntax
    = Select 
    | Where
    | Identifier String

derive instance syntaxGeneric :: Generic Syntax _
instance syntaxShow :: Show Syntax where 
    show = genericShow