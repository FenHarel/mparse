module Combinators (spec) where

import Control.Applicative (Alternative ((<|>)))
import qualified Mparse.EParser as EP
import Mparse.MParser ((//))
import qualified Mparse.MParser as MP
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser combinator primitives. Used to build complex parsers" $ do
    it "string matches an exact string" $ do
      EP.eparse (MP.exact "hello") "hello world" `shouldBe` MP.ParsedData (" world", (0, 5, 5), "hello")
      EP.eparse (MP.exact "hello") "hell no" `shouldBe` MP.ParserError [("hell no", (0, 0, 0), "Expected the string: 'hello'"), ("no", (0, 5, 5), "Received character: ' '")]
      EP.eparse (MP.exact "hello") "hell no" `shouldBe` MP.ParserError [("hell no", (0, 0, 0), "Expected the string: 'hello'"), ("no", (0, 5, 5), "Received character: ' '")]

    it "repeated and repeated1 collect multiple occurrences" $ do
      EP.eparse (MP.repeated1 (MP.char 'a')) "aaabc" `shouldBe` MP.ParsedData ("bc", (0, 3, 3), replicate 3 'a')
      EP.eparse (MP.repeated (MP.char 'a')) "aaabc" `shouldBe` MP.ParsedData ("bc", (0, 3, 3), replicate 3 'a')
      EP.eparse (MP.repeated1 (MP.char 'a')) "xyz" `shouldBe` MP.ParserError [("yz", (0, 1, 1), "Received character: 'x'")]

    it "nat parses natural numbers" $ do
      EP.eparse MP.nat "123abc" `shouldBe` MP.ParsedData ("abc", (0, 3, 3), 123)
      EP.eparse MP.nat "abc" `shouldBe` MP.ParserError [("abc", (0, 0, 0), "Expected numeric characters"), ("bc", (0, 1, 1), "Received character: 'a'")]

    it "alternative (<|>) tries parsers in order" $ do
      EP.eparse (MP.char 'a' <|> MP.char 'b') "abc" `shouldBe` MP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse (MP.char 'a' <|> MP.char 'b') "bcd" `shouldBe` MP.ParsedData ("cd", (0, 1, 1), 'b')
      EP.eparse (MP.char 'a' <|> MP.char 'b') "xyz" `shouldBe` MP.ParserError [("yz", (0, 1, 1), "Received character: 'x'")]

    it "pairOn parses values separated by another parser" $ do
      EP.eparse (MP.pairOn (MP.char '=') (MP.repeated1 MP.letter) MP.nat) "name=42" `shouldBe` MP.ParsedData ("", (0, 7, 7), ("name", 42))

    it "sepby parses items separated by a delimiter" $ do
      EP.eparse (MP.digit // MP.char ',') "1,2,3" `shouldBe` MP.ParsedData ("", (0, 5, 5), ['1', '2', '3'])
      EP.eparse (MP.digit // MP.char ',') "1" `shouldBe` MP.ParsedData ("", (0, 1, 1), ['1'])

    it "between parses content between delimiters" $ do
      EP.eparse (MP.between (MP.char '(') (MP.char ')') (MP.repeated1 MP.letter)) "(abc)" `shouldBe` MP.ParsedData ("", (0, 5, 5), "abc")
