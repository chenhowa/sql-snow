module Parser.Common 
    ( ParseTree(..)
    , Syntax(..)
    , InputStream 
    , Output
    , Parser
    , SyntaxParser
    , SyntaxArrayParser
    ) where 

import Data.Identity
import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List as L
import Text.Parsing.Parser as P
import Tokenizer.Tokens (Token(..))


data Syntax
    = Select 
    | Where
    | Identifier String
    | Comma
    | Wildcard
    | Multiply
    | Plus 
    | Minus
    | FloatDivide 
    | Modulo 
    | Equals 
    | NotEquals
    | Not 
    | And 
    | Or 
    | LT 
    | LTE
    | GT 
    | GTE
    | Constant String

derive instance syntaxGeneric :: Generic Syntax _
instance syntaxShow :: Show Syntax where 
    show = genericShow
instance syntaxEq :: Eq Syntax where
    eq = genericEq

data ParseTree a
    = Leaf a
    | Node a (L.List (ParseTree a))

instance ptShow :: Show a => Show (ParseTree a) where
    show tree = case tree of 
        Leaf a -> "Leaf " <> show a
        Node a list -> "Node " <> show a <> " " <> "(" <> (children list) <> ")"

        where
            children l = foldl  add "" l
            add str item = str <> (show item) <> ","

instance ptEq :: Eq a => Eq (ParseTree a) where 
    eq (Leaf one) (Leaf two) = one == two
    eq (Node _ _) (Leaf _) = false
    eq (Leaf _) (Node _ _) = false
    eq (Node val1 c1) (Node val2 c2) = (val1 == val2) && (c1 == c2)




type InputStream = Array Token
type Output = ParseTree Syntax
type Parser = P.ParserT InputStream Identity Output
type SyntaxParser = P.ParserT InputStream Identity Syntax
type SyntaxArrayParser = P.ParserT InputStream Identity (Array Syntax)


