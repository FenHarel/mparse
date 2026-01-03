module EParserSpec (spec) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import Mparse.EParser ((//))
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
      EP.uparse (EP.digit) "abc" `shouldBe` [EP.Failure (EP.RootError "Received character: 'a'" 1)]

    it "char matches a specific character" $ do
      EP.uparse (EP.char 'a') "abc" `shouldBe` [EP.Success ('a', "bc", 1)]
      EP.uparse (EP.char 'a') "xyz" `shouldBe` [EP.Failure (EP.RootError "Received character: 'x'" 1)]

    it "string matches an exact string" $ do
      EP.uparse (EP.exact "hello") "hello world" `shouldBe` [EP.Success ("hello", " world", 5)]
      EP.uparse (EP.exact "hello") "hell no" `shouldBe` [EP.Failure (EP.ParentError "Expected the string: 'hello'" 0 (EP.RootError "Received character: ' '" 5))]
      EP.expects (EP.exact "hello") `shouldBe` (EP.Expectation "Expected the string: 'hello'")
  describe "combinators" $ do
    it "many' collects zero or more occurrences" $ do
      EP.uparse (EP.repeated1 (EP.char 'a')) "aaabc" `shouldBe` [EP.Success (replicate 3 'a', "bc", 3)]
      EP.uparse (EP.repeated (EP.char 'a')) "aaabc" `shouldBe` [EP.Success (replicate 3 'a', "bc", 3)]
      EP.uparse (EP.repeated1 (EP.char 'a')) "xyz" `shouldBe` [EP.Failure (EP.RootError "Received character: 'x'" 1)]

    it "alternative (<|>) tries parsers in order" $ do
      EP.uparse (EP.char 'a' <|> EP.char 'b') "abc" `shouldBe` [EP.Success ('a', "bc", 1)]
      EP.uparse (EP.char 'a' <|> EP.char 'b') "bcd" `shouldBe` [EP.Success ('b', "cd", 1)]
      EP.uparse (EP.char 'a' <|> EP.char 'b') "xyz" `shouldBe` [EP.Failure (EP.RootError "Received character: 'x'" 1)]

    it "sepby parses items separated by a delimiter" $ do
      EP.uparse (EP.digit // EP.char ',') "1,2,3" `shouldBe` [EP.Success (['1', '2', '3'], "", 5)]
      EP.uparse (EP.digit // EP.char ',') "1" `shouldBe` [EP.Success (['1'], "", 1)]

    it "bracket parses content between delimiters" $ do
      EP.uparse (EP.bracket (EP.char '(') (EP.char ')') (EP.repeated1 EP.letter)) "(abc)" `shouldBe` [EP.Success ("abc", "", 5)]

  describe "complex parsers" $ do
    it "nat parses natural numbers" $ do
      EP.uparse EP.nat "123abc" `shouldBe` [EP.Success (123, "abc", 3)]
      EP.uparse EP.nat "abc" `shouldBe` [EP.Failure (EP.ParentError "Expected numeric characters" 0 (EP.RootError "Received character: 'a'" 1))]

    it "spaces consumes whitespace" $ do
      EP.uparse EP.spaces "   abc" `shouldBe` [EP.Success ("   ", "abc", 3)]

    it "token consumes trailing whitespace" $ do
      EP.uparse (EP.token (EP.char 'a')) "a   bc" `shouldBe` [EP.Success ('a', "bc", 4)]

    it "between parses values separated by another parser" $ do
      EP.uparse (EP.between (EP.char '=') (EP.repeated1 EP.letter) EP.nat) "name=42" `shouldBe` [EP.Success (("name", 42), "", 7)]

    it "optional makes a parser optional" $ do
      EP.uparse (EP.optional (EP.char 'a')) "abc" `shouldBe` [EP.Success (Just 'a', "bc", 1)]
      EP.uparse (EP.optional (EP.char 'a')) "xyz" `shouldBe` [EP.Success (Nothing, "xyz", 0)]
