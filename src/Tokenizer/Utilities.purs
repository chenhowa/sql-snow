module Tokenizer.Utilities 
    ( caseInsensitive
    , toUpper 
    , toLower
    , runCaseInsensitive
    , whiteSpace
    , skipSpaces
    ) where 

import Prelude

import Control.Alt ((<|>))
import Data.Array (head, reverse)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S
import Data.Identity



caseInsensitive :: forall s m. S.StringLike s => Monad m => String -> P.ParserT s m String
caseInsensitive str = 
    let arr =  reverse <<< toCharArray $ str
        caseInsensitiveParser charac = S.char (toLower charac) <|> S.char (toUpper charac)
        charParsers = caseInsensitiveParser <$> arr
    in  foldr combineParsers (S.string "") charParsers
    where 
        combineParsers :: forall a b. S.StringLike a => Monad b => P.ParserT a b Char -> P.ParserT a b String -> P.ParserT a b String
        combineParsers charP stringP = do 
            string <- stringP
            c <- charP
            let full = String.joinWith "" [string, fromCharArray [c]]
            pure full

toLower :: Char -> Char
toLower char = 
    let lowercase = String.toLower $ fromCharArray [char]
        c = case head $ toCharArray lowercase of 
                Just lower -> lower
                Nothing -> char
    in c

toUpper :: Char -> Char
toUpper char = 
    let uppercase = String.toUpper $ fromCharArray [char]
        c = case head $ toCharArray uppercase of 
                Just upper -> upper 
                Nothing -> char
    in c

runCaseInsensitive :: String -> String -> Either P.ParseError String 
runCaseInsensitive str matcher = 
    let Identity r = P.runParserT str $ caseInsensitive matcher
    in  r

whiteSpace :: forall s m. S.StringLike s => Monad m => P.ParserT s m String
whiteSpace = do 
    spaces <- A.some $ S.oneOf [' ', '\n', '\t', '\r']
    pure $ fromCharArray spaces

skipSpaces :: forall s m. S.StringLike s => Monad m => P.ParserT s m Unit
skipSpaces = do 
    _ <- whiteSpace
    pure unit 