module Parser.Parsing 
    ( class ArrayLike
    , drop 
    , findIndex 
    , null 
    , uncons
    ) where 

import Prelude

import Control.Monad.State as ST
import Data.Array as A
import Data.Maybe (Maybe(..))
import Parser.Common (ParseTree(..))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String as S



class ArrayLike f where 
    drop :: forall a. Int -> f a -> f a
    findIndex :: forall a. (a -> Boolean) -> f a -> Maybe Int
    null :: forall a. f a -> Boolean
    uncons :: forall a. f a -> Maybe { head :: a, tail :: f a }


instance arrayLikeArray :: ArrayLike Array where 
    drop =  A.drop
    findIndex = A.findIndex
    null = A.null
    uncons = A.uncons

anyToken :: forall f a g. ArrayLike f => Monad g => P.ParserT (f a) g a
anyToken = do 
    input <- ST.gets \(P.ParseState input _ _) -> input
    case uncons input of
        Nothing -> P.fail "Unexpected EOF"
        Just { head, tail } -> do
            ST.modify_ \(P.ParseState _ position _) ->
                P.ParseState tail
                    (incPositionArray position 1)
                    true
            pure head

satisfy :: forall f a g. ArrayLike f => Show a => Monad g => (a -> Boolean) -> P.ParserT (f a) g a
satisfy pred = C.tryRethrow do
  t <- anyToken
  if pred t then pure t
         else P.fail $ "Token '" <> show t <> "' did not satisfy predicate"


token :: forall f a g. ArrayLike f => Eq a => Show a => Monad g => a -> P.ParserT (f a) g a
token t = satisfy (_ == t) <?> show t

incPositionArray :: Position -> Int -> Position
incPositionArray (Position old) inc = Position $ old { column = (old.column + inc) } 