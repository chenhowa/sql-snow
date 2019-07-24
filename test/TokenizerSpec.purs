module Test.TokenizerSpec where 

import Data.Identity
import Prelude

import Data.Either as Either
import Data.String (length)
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Text.Parsing.Parser as P
import Tokenizer as T
import Tokenizer.Identifiers as I
import Tokenizer.Keywords as K
import Tokenizer.Tokens (Token(..))


spec :: Spec Unit
spec = describe "Testing tokenizer" do
    describe "individual token parsing" do
        describe "subquery" do 
            it "mix of keywords, identifiers, and constants" do 
                P.runParserT "(SELECT hello 5 )" T.subQuery `shouldEqual` Identity (Either.Right $ [LeftParen, Select, Identifier "hello", Constant "5", RightParen])
               -- T.tokenize "(SELECT hello 5 )" `shouldEqual` (Either.Right $ [LeftParen, Select, Identifier "hello", Constant "5", RightParen])
        describe "function" do 
            it "with one arg" do 
                T.tokenize "COUNT  (sid)" `shouldEqual` (Either.Right $ [Identifier "COUNT", LeftParen, Identifier "sid", RightParen])
            it "with two args" do 
                T.tokenize "COUNT(  sid  , bid )" `shouldEqual` (Either.Right $ 
                                            [ Identifier "COUNT", LeftParen, Identifier "sid", Identifier "bid", RightParen])
        describe "comments" do
            describe "line" do
                let comment = "  whatever it takes "
                it "followed by newline" do 
                    T.runToken ("--" <> comment <> "\n") `shouldEqual` (Either.Right $ LineComment comment)
                it "followed by eof" do
                    T.runToken ("--" <> comment) `shouldEqual` (Either.Right $ LineComment comment)
            it "block" do 
                let comment = "  part  \n of \n   a \t healthy"
                T.runToken ("/*" <> comment <> "*/") `shouldEqual` (Either.Right $ BlockComment comment)
        describe "punctuation" do 
            it "left parentheses" do 
                T.runToken "(" `shouldEqual` (Either.Right $ LeftParen)
            it "right parentheses" do 
                T.runToken ")" `shouldEqual` (Either.Right $ RightParen)
            it "comma" do 
                T.runToken "," `shouldEqual` (Either.Right Comma)
        describe "constants" do 
            it "decimal without commas" do
                (T.runToken "5") `shouldEqual` (Either.Right $ Constant "5")
        describe "identifiers" do 
            it "letters" do 
                (T.runToken "hello") `shouldEqual` (Either.Right $ Identifier "hello")
            it "letters and numbers" do 
                (T.runToken "he88o") `shouldEqual` (Either.Right $ Identifier "he88o")
            it "letters, numbers, and underscores" do 
                (T.runToken "he8_8o_") `shouldEqual` (Either.Right $ Identifier "he8_8o_")
            it "rejects identifiers that don't start with a letter" do 
                (T.tokenize "_hello") `shouldSatisfy` isError
                (T.tokenize "1hello") `shouldSatisfy` isError
            it "rejects identifiers with special characters" do 
                (T.tokenize "he$lo") `shouldSatisfy` isError
        describe "keywords" do 
            it "select" do 
                (T.runToken "select") `shouldEqual` (Either.Right Select)
            it "from" do 
                (T.runToken "from") `shouldEqual` (Either.Right From)
            it "where" do 
                (T.runToken "where") `shouldEqual` (Either.Right Where)
            it "group by" do 
                (T.runToken "group by") `shouldEqual` (Either.Right GroupBy)
            it "having" do 
                (T.runToken "having") `shouldEqual` (Either.Right Having)
            it "in" do 
                T.runToken "in" `shouldEqual` (Either.Right In)
            it "distinct" do 
                T.runToken "distinct" `shouldEqual` (Either.Right Distinct)
            it "limit" do 
                T.runToken "limit" `shouldEqual` (Either.Right Limit)
            it "order by" do 
                T.runToken "order by" `shouldEqual` (Either.Right OrderBy)
            it "asc" do 
                T.runToken "asc" `shouldEqual` (Either.Right Ascending)
            it "desc" do 
                T.runToken "desc" `shouldEqual` (Either.Right Descending)
            it "union" do 
                T.runToken "union" `shouldEqual` (Either.Right Union)
            it "intersect" do 
                T.runToken "intersect" `shouldEqual` (Either.Right Intersect)
            it "all" do 
                T.runToken "all" `shouldEqual` (Either.Right All)
            it "left" do 
                T.runToken "left" `shouldEqual` (Either.Right Left)
            it "right" do 
                T.runToken "right" `shouldEqual` (Either.Right Right)
            it "inner" do 
                T.runToken "inner" `shouldEqual` (Either.Right Inner)
            it "outer" do 
                T.runToken "outer" `shouldEqual` (Either.Right Outer)
            it "natural" do 
                T.runToken "natural" `shouldEqual` (Either.Right Natural)
            it "join" do
                T.runToken "join" `shouldEqual` (Either.Right Join)
            it "on" do 
                T.runToken "on" `shouldEqual` (Either.Right On)
            it "as" do 
                T.runToken "as" `shouldEqual` (Either.Right As)
    describe "all tokens together" do 
        it "keyword tokens" do 
            let input = "SELECT FROM WHERE GROUP BY HAVING IN DISTINCT LIMIT ORDER BY ASC DESC UNION INTERSECT ALL LEFT RIGHT" <>
                        " INNER OUTER NATURAL JOIN ON iDenTifier AS"
                result = T.tokenize input
            result `shouldEqual` Either.Right
                    [ Select
                    , From
                    , Where
                    , GroupBy
                    , Having
                    , In 
                    , Distinct 
                    , Limit 
                    , OrderBy
                    , Ascending
                    , Descending
                    , Union
                    , Intersect
                    , All
                    , Left 
                    , Right 
                    , Inner
                    , Outer 
                    , Natural
                    , Join
                    , On
                    , Identifier "iDenTifier"
                    , As
                    ]
        it "allows whitespace at the beginning and end" do
            let input = "  SELECT FROM WHERE GROUP BY   "
                result = T.tokenize input
            result `shouldEqual` Either.Right [ Select, From, Where, GroupBy ]
        it "requires whitespace between tokens" do 
            let input = "SELECTFROM"
                result = T.tokenize input
        
            result `shouldSatisfy` isError

isError :: forall a b. Either.Either a b -> Boolean 
isError either = case either of 
            Either.Left _ -> true 
            _ -> false