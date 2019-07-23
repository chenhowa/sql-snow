module Tokenizer.Utilities 
    ( caseInsensitive
    , toUpper 
    , toLower

    ) where 

import Prelude
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S
import Data.String as String
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Control.Alt ((<|>))
import Data.Foldable (foldr)
import Data.Array (head)
import Data.Maybe (Maybe(..))



caseInsensitive :: forall s m. S.StringLike s => Monad m => String -> P.ParserT s m String
caseInsensitive str = 
    let arr = toCharArray str
        caseInsensitiveParser charac = S.char (toLower charac) <|> S.char (toUpper charac)
        charParsers = caseInsensitiveParser <$> arr
    in  foldr combineParsers mempty charParsers
    where 
        combineParsers :: forall a b. S.StringLike a => Monad b => P.ParserT a b Char -> P.ParserT a b String -> P.ParserT a b String
        combineParsers charP stringP = do 
            string <- stringP
            c <- charP
            let full = String.joinWith "" [str, fromCharArray [c]]
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
