module ParserSpec (spec) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import Mparse.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "Basic parsers" $ do
    it "result returns a value without consuming input" $ do
      parse (result (42 :: Int)) "hello" `shouldBe` [(42, "hello")]

    it "zero always fails" $ do
      parse zero "anything" `shouldBe` ([] :: [(Int, String)])

    it "item consumes one character" $ do
      parse item "abc" `shouldBe` [('a', "bc")]
      parse item "" `shouldBe` []

  describe "Character parsers" $ do
    it "sat matches characters that satisfy a predicate" $ do
      parse (sat isDigit) "123" `shouldBe` [('1', "23")]
      parse (sat isDigit) "abc" `shouldBe` []

    it "char matches a specific character" $ do
      parse (char 'a') "abc" `shouldBe` [('a', "bc")]
      parse (char 'a') "xyz" `shouldBe` []

    it "string matches an exact string" $ do
      parse (string "hello") "hello world" `shouldBe` [("hello", " world")]
      parse (string "hello") "hell no" `shouldBe` []

  describe "Combinators" $ do
    it "many' collects zero or more occurrences" $ do
      parse (many' (char 'a')) "aaabc" `shouldBe` [(replicate 3 'a', "bc")]
      parse (many' (char 'a')) "xyz" `shouldBe` [("", "xyz")]

    it "many1 requires at least one occurrence" $ do
      parse (many1 (char 'a')) "aaabc" `shouldBe` [(replicate 3 'a', "bc")]
      parse (many1 (char 'a')) "xyz" `shouldBe` []

    it "Alternative (<|>) tries parsers in order" $ do
      parse (char 'a' <|> char 'b') "abc" `shouldBe` [('a', "bc")]
      parse (char 'a' <|> char 'b') "bcd" `shouldBe` [('b', "cd")]
      parse (char 'a' <|> char 'b') "xyz" `shouldBe` []

    it "sepBy parses items separated by a delimiter" $ do
      parse (digit `sepBy` char ',') "1,2,3" `shouldBe` [(['1', '2', '3'], "")]
      parse (digit `sepBy` char ',') "1" `shouldBe` [(['1'], "")]

    it "bracket parses content between delimiters" $ do
      parse (bracket (char '(') (many1 letter) (char ')')) "(abc)" `shouldBe` [("abc", "")]

  describe "Complex parsers" $ do
    it "nat parses natural numbers" $ do
      parse nat "123abc" `shouldBe` [(123, "abc")]
      parse nat "abc" `shouldBe` []

    it "ident parses identifiers" $ do
      parse ident "abc123" `shouldBe` [("abc123", "")]
      parse ident "_var" `shouldBe` [("_var", "")]
      parse ident "123abc" `shouldBe` []

    it "spaces consumes whitespace" $ do
      parse spaces "   abc" `shouldBe` [((), "abc")]

    it "token consumes trailing whitespace" $ do
      parse (token (char 'a')) "a   bc" `shouldBe` [('a', "bc")]

    it "between parses values separated by another parser" $ do
      parse (between (char '=') ident nat) "name=42" `shouldBe` [(("name", 42), "")]

    it "zeroOrOne makes a parser optional" $ do
      parse (zeroOrOne (char 'a')) "abc" `shouldBe` [(Just 'a', "bc")]
      parse (zeroOrOne (char 'a')) "xyz" `shouldBe` [(Nothing, "xyz")]

    it "pair combines two characters into a string" $ do
      parse (pair (char 'a') (char 'b')) "abc" `shouldBe` [("ab", "c")]

    it "parsedValue extracts values from successful parses" $ do
      parsedValue nat "123" `shouldBe` Just 123
      parsedValue nat "abc" `shouldBe` Nothing
