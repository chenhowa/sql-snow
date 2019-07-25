module Test.ClausesSpec where 

import Data.Identity
import Prelude

import Data.Either as Either
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser as P
import Data.List as L

import Parser.Parsing as Parser
import Tokenizer.Tokens (Token(..))
import Parser.Clauses as CL
import Parser.Common as CO

spec :: Spec Unit
spec = describe "Clause parsing" do
    describe "select" do 
        it "only columns" do 
            P.runParserT [ Select, Identifier "sid", Comma, Identifier "bid" ] CL.select 
                `shouldEqual` Identity 
                    ( Either.Right
                        ( CO.Node CO.Select $ L.fromFoldable (CO.Leaf <$> [ CO.Identifier "sid", CO.Identifier "bid"] )

                        )
                    )
        it "wildcard" do 
            P.runParserT [ Select, Asterisk ] CL.select 
                `shouldEqual` Identity 
                    ( Either.Right
                        ( CO.Node CO.Select $ L.fromFoldable (CO.Leaf <$> [ CO.Wildcard ] )

                        )
                    )