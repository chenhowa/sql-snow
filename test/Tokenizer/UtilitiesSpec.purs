module Test.Tokenizer.UtilitiesSpec where 

import Prelude

import Data.Either as Either
import Data.String (length)
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Tokenizer as T
import Tokenizer.Utilities as U
import Data.Either (Either(..))

spec :: Spec Unit
spec = describe "Utilities" do 
    it "toLower" do 
        U.toLower 'C' `shouldEqual` 'c'
        U.toLower 'c' `shouldEqual` 'c'
        U.toLower '&' `shouldEqual` '&'
    it "toUpper" do 
        U.toUpper 'c' `shouldEqual` 'C' 
        U.toUpper 'C' `shouldEqual` 'C'
        U.toUpper '&' `shouldEqual` '&'
    describe "caseInsensitive parser" do 
        it "upper case" do 
            U.runCaseInsensitive "what" "WHAT" `shouldEqual` Right "what"
        it "mixed case" do 
            U.runCaseInsensitive "wHaT" "WhAt" `shouldEqual` Right "wHaT"