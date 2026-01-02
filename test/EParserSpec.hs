module EParserSpec (spec) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import qualified Mparse.EParser as EP
import Test.Hspec

spec :: Spec
spec = do
  describe "Basic parsers" $ do
    it "result returns a value without consuming input" $ do
      EP.uparse (EP.result (42 :: Int)) "hello" `shouldBe` [EP.Success (42, "hello", 0)]

    it "zero always fails" $ do
      EP.uparse EP.zero "anything" `shouldBe` ([] :: [EP.ParseResult Int])

    it "item consumes one character" $ do
      EP.uparse EP.item "abc" `shouldBe` [EP.Success ('a', "bc", 1)]
      EP.uparse EP.item "" `shouldBe` [EP.Failure (EP.RootError "Unexpected end of input." 0)]

  describe "Character parsers" $ do
    it "sat matches characters that satisfy a predicate" $ do
      EP.uparse (EP.sat isDigit) "123" `shouldBe` [EP.Success ('1', "23", 1)]
      EP.uparse (EP.sat isDigit) "abc" `shouldBe` [EP.Failure (EP.RootError "Received character: 'a'" 1)]

    it "sat matches characters that satisfy a predicate" $ do
      EP.uparse (EP.digit) "123" `shouldBe` [EP.Success ('1', "23", 1)]
      EP.uparse (EP.digit) "abc" `shouldBe` [EP.Failure (EP.ParentError "Expected numeric digit." 0 (EP.RootError "Received character: 'a'" 1))]

    it "char matches a specific character" $ do
      -- EP.uparse (EP.char 'a') "abc" `shouldBe` []
      EP.uparse (EP.char 'a') "abc" `shouldBe` [EP.Success ('a', "bc", 1)]
      EP.uparse (EP.char 'a') "xyz" `shouldBe` [EP.Failure (EP.RootError "Received character: 'x'" 1)]

    it "string matches an exact string" $ do
      EP.expects (EP.char' (EP.Expectation "sdfsdf") 'c') `shouldBe` (EP.Expectation "sdfsdf")
      EP.uparse (EP.string "hello") "hello world" `shouldBe` [EP.Success ("hello", " world", 5)]
      EP.uparse (EP.string "hello") "hell no" `shouldBe` [EP.Failure (EP.ParentError "Expected the string: 'hello'" 0 (EP.RootError "Received character: ' '" 5))]
      EP.expects (EP.string "hello") `shouldBe` (EP.Expectation "Expected the string: 'hello'")
    it "asdfsdf" $
      EP.printErrorTrace
        (EP.ParentError "Expected the string: 'hello'" 0 (EP.ParentError "Expected the string: 'hello'" 3 (EP.ParentError "Expected the string: 'hello'" 3 (EP.ParentError "Expected the string: 'hello'" 3 (EP.RootError "Received character: ' '" 5)))))
        >>= shouldBe [()]

-- describe "combinators" $ do
--     it "many' collects zero or more occurrences" $ do
--     parse (many' (char 'a')) "aaabc" `shouldbe` [(replicate 3 'a', "bc")]
--     parse (many' (char 'a')) "xyz" `shouldbe` [("", "xyz")]

--     it "many1 requires at least one occurrence" $ do
--     parse (many1 (char 'a')) "aaabc" `shouldbe` [(replicate 3 'a', "bc")]
--     parse (many1 (char 'a')) "xyz" `shouldbe` []

--     it "alternative (<|>) tries parsers in order" $ do
--     parse (char 'a' <|> char 'b') "abc" `shouldbe` [('a', "bc")]
--     parse (char 'a' <|> char 'b') "bcd" `shouldbe` [('b', "cd")]
--     parse (char 'a' <|> char 'b') "xyz" `shouldbe` []

--     it "sepby parses items separated by a delimiter" $ do
--     parse (digit `sepby` char ',') "1,2,3" `shouldbe` [(['1', '2', '3'], "")]
--     parse (digit `sepby` char ',') "1" `shouldbe` [(['1'], "")]

--     it "bracket parses content between delimiters" $ do
--     parse (bracket (char '(') (many1 letter) (char ')')) "(abc)" `shouldbe` [("abc", "")]

-- describe "complex parsers" $ do
--     it "nat parses natural numbers" $ do
--     parse nat "123abc" `shouldbe` [(123, "abc")]
--     parse nat "abc" `shouldbe` []

--     it "ident parses identifiers" $ do
--     parse ident "abc123" `shouldbe` [("abc123", "")]
--     parse ident "_var" `shouldbe` [("_var", "")]
--     parse ident "123abc" `shouldbe` []

--     it "spaces consumes whitespace" $ do
--     parse spaces "   abc" `shouldbe` [((), "abc")]

--     it "token consumes trailing whitespace" $ do
--     parse (token (char 'a')) "a   bc" `shouldbe` [('a', "bc")]

--     it "between parses values separated by another parser" $ do
--     parse (between (char '=') ident nat) "name=42" `shouldbe` [(("name", 42), "")]

--     it "zeroorone makes a parser optional" $ do
--     parse (zeroorone (char 'a')) "abc" `shouldbe` [(just 'a', "bc")]
--     parse (zeroorone (char 'a')) "xyz" `shouldbe` [(nothing, "xyz")]

--     it "pair combines two characters into a string" $ do
--     parse (pair (char 'a') (char 'b')) "abc" `shouldbe` [("ab", "c")]

--     it "parsedvalue extracts values from successful parses" $ do
--     parsedvalue nat "123" `shouldbe` just 123
--     parsedvalue nat "abc" `shouldbe` nothing
