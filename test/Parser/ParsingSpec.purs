module Test.Parser.ParsingSpec where 

import Data.Identity
import Prelude

import Data.Either as Either
import Data.String (length)
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Text.Parsing.Parser as P

import Parser.Parsing as Parser
import Data.List as L

spec :: Spec Unit
spec = describe "Testing parsing functions" do
    describe "for arrays" do 
        it "anyToken" do 
            P.runParserT [ 1 ] Parser.anyToken `shouldEqual` Identity (Either.Right 1)
        it "satisfy" do 
            P.runParserT [ 1 ] (Parser.satisfy \token -> token == 1) `shouldEqual` Identity (Either.Right 1)
        it "token" do 
            P.runParserT [ 1 ] (Parser.token 1) `shouldEqual` Identity (Either.Right 1)
        it "eof" do 
            P.runParserT [] (Parser.eof :: P.ParserT (Array Int) Identity Unit) `shouldEqual` (Identity (Either.Right unit))
        it "oneOf" do 
            P.runParserT [2] (Parser.oneOf [1, 2]) `shouldEqual` Identity (Either.Right 2)
        it "noneOf" do
            P.runParserT [3] (Parser.noneOf [1, 2]) `shouldEqual` Identity (Either.Right 3)
    describe "for lists" do
        it "anyToken" do 
            P.runParserT (L.fromFoldable [ 1 ]) Parser.anyToken `shouldEqual` Identity (Either.Right 1)
        it "satisfy" do 
            P.runParserT (L.fromFoldable [ 1 ]) (Parser.satisfy \token -> token == 1) `shouldEqual` Identity (Either.Right 1)
        it "token" do 
            P.runParserT (L.fromFoldable [ 1 ]) (Parser.token 1) `shouldEqual` Identity (Either.Right 1)
        it "eof" do 
            P.runParserT (L.fromFoldable []) (Parser.eof :: P.ParserT (L.List Int) Identity Unit) `shouldEqual` (Identity (Either.Right unit))
        it "oneOf" do 
            P.runParserT (L.fromFoldable [2]) (Parser.oneOf [1, 2]) `shouldEqual` Identity (Either.Right 2)
        it "noneOf" do
            P.runParserT (L.fromFoldable [3]) (Parser.noneOf [1, 2]) `shouldEqual` Identity (Either.Right 3)