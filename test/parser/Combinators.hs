module Combinators (spec) where

import Control.Applicative (Alternative ((<|>)))
import Mparse.EParser ((//))
import qualified Mparse.EParser as EP
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser combinator primitives. Used to build complex parsers" $ do
    it "string matches an exact string" $ do
      EP.parseResults (EP.exact "hello") "hello world" `shouldBe` [EP.Success ("hello", (" world", (0, 5, 5)))]
      EP.parseResults (EP.exact "hello") "hell no" `shouldBe` [EP.Failure (EP.ParentError ("Expected the string: 'hello'", (0, 0, 0)) (EP.RootError ("Received character: ' '", (0, 5, 5))))]
      EP.expects (EP.exact "hello") `shouldBe` (EP.Expectation "Expected the string: 'hello'")

    it "repeated and repeated1 collect multiple occurrences" $ do
      EP.parseResults (EP.repeated1 (EP.char 'a')) "aaabc" `shouldBe` [EP.Success (replicate 3 'a', ("bc", (0, 3, 3)))]
      EP.parseResults (EP.repeated (EP.char 'a')) "aaabc" `shouldBe` [EP.Success (replicate 3 'a', ("bc", (0, 3, 3)))]
      EP.parseResults (EP.repeated1 (EP.char 'a')) "xyz" `shouldBe` [EP.Failure (EP.RootError ("Received character: 'x'", (0, 1, 1)))]

    it "nat parses natural numbers" $ do
      EP.parseResults EP.nat "123abc" `shouldBe` [EP.Success (123, ("abc", (0, 3, 3)))]
      EP.parseResults EP.nat "abc" `shouldBe` [EP.Failure (EP.ParentError ("Expected numeric characters", (0, 0, 0)) (EP.RootError ("Received character: 'a'", (0, 1, 1))))]

    it "alternative (<|>) tries parsers in order" $ do
      EP.parseResults (EP.char 'a' <|> EP.char 'b') "abc" `shouldBe` [EP.Success ('a', ("bc", (0, 1, 1)))]
      EP.parseResults (EP.char 'a' <|> EP.char 'b') "bcd" `shouldBe` [EP.Success ('b', ("cd", (0, 1, 1)))]
      EP.parseResults (EP.char 'a' <|> EP.char 'b') "xyz" `shouldBe` [EP.Failure (EP.RootError ("Received character: 'x'", (0, 1, 1)))]

    it "between parses values separated by another parser" $ do
      EP.parseResults (EP.between (EP.char '=') (EP.repeated1 EP.letter) EP.nat) "name=42" `shouldBe` [EP.Success (("name", 42), ("", (0, 7, 7)))]

    it "sepby parses items separated by a delimiter" $ do
      EP.parseResults (EP.digit // EP.char ',') "1,2,3" `shouldBe` [EP.Success (['1', '2', '3'], ("", (0, 5, 5)))]
      EP.parseResults (EP.digit // EP.char ',') "1" `shouldBe` [EP.Success (['1'], ("", (0, 1, 1)))]

    it "bracket parses content between delimiters" $ do
      EP.parseResults (EP.bracket (EP.char '(') (EP.char ')') (EP.repeated1 EP.letter)) "(abc)" `shouldBe` [EP.Success ("abc", ("", (0, 5, 5)))]
