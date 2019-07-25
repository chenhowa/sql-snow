module Parser.Parsing 
    ( class StreamLike
    , drop 
    --, findIndex 
    , null 
    , uncons
    , anyToken
    , satisfy
    , token
    , eof
    , oneOf
    , noneOf
    ) where 

import Prelude

import Control.Monad.State as ST
import Data.Array as A
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Pos (Position(..))
import Data.List as L
import Data.Sequence as S
import Data.Tuple as T

import Data.Foldable (elem, notElem)

class StreamLike f where 
    drop :: forall a. Int -> f a -> f a
    --findIndex :: forall a. (a -> Boolean) -> f a -> Maybe Int
    null :: forall a. f a -> Boolean
    uncons :: forall a. f a -> Maybe { head :: a, tail :: f a }


instance arrayLikeStream :: StreamLike Array where 
    drop =  A.drop
    --findIndex = A.findIndex
    null = A.null
    uncons = A.uncons

instance listLikeStream :: StreamLike L.List where 
    drop = L.drop 
    --findIndex = L.findIndex 
    null = L.null 
    uncons = L.uncons

instance sequenceLikeStream :: StreamLike S.Seq where 
    drop = S.drop 
    --findIndex = S.findIndex 
    null = S.null 
    uncons seq = case S.uncons seq of 
        Nothing -> Nothing
        Just tuple -> Just { head: T.fst tuple, tail: T.snd tuple }

anyToken :: forall f a m. StreamLike f => Monad m => P.ParserT (f a) m a
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

satisfy :: forall f a m. StreamLike f => Show a => Monad m => (a -> Boolean) -> P.ParserT (f a) m a
satisfy pred = C.tryRethrow do
  t <- anyToken
  if pred t then pure t
         else P.fail $ "Token '" <> show t <> "' did not satisfy predicate"


token :: forall f a m. StreamLike f => Eq a => Show a => Monad m => a -> P.ParserT (f a) m a
token t = satisfy (_ == t) <?> show t

incPositionArray :: Position -> Int -> Position
incPositionArray (Position old) inc = Position $ old { column = (old.column + inc) } 

eof :: forall f a m. StreamLike f => Eq a => Show a => Monad m => P.ParserT (f a) m Unit
eof = do
    input <- ST.gets \(P.ParseState input _ _) -> input
    unless (null input) (P.fail "Expected EOF")

oneOf :: forall f a m. StreamLike f => Show a => Eq a => Monad m => Array a -> P.ParserT (f a) m a
oneOf ss = satisfy (flip elem ss) <?> ("one of " <> show ss)

noneOf :: forall f a m. StreamLike f => Show a => Eq a => Monad m => Array a -> P.ParserT (f a) m a
noneOf ss = satisfy (flip notElem ss) <?> ("none of " <> show ss)