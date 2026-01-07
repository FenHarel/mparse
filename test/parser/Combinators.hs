module Combinators (spec) where

import Control.Applicative (Alternative ((<|>)))
import qualified Mparse.EParser as EP
import Mparse.GeneralParser ((//))
import qualified Mparse.GeneralParser as GP
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser combinator primitives. Used to build complex parsers" $ do
    it "string matches an exact string" $ do
      EP.eparse (GP.exact "hello") "hello world" `shouldBe` GP.ParsedData (" world", (0, 5, 5), "hello")
      EP.eparse (GP.exact "hello") "hell no" `shouldBe` GP.ParserError [("hell no", (0, 0, 0), "Expected the string: 'hello'"), ("no", (0, 5, 5), "Received character: ' '")]
      EP.eparse (GP.exact "hello") "hell no" `shouldBe` GP.ParserError [("hell no", (0, 0, 0), "Expected the string: 'hello'"), ("no", (0, 5, 5), "Received character: ' '")]

    it "repeated and repeated1 collect multiple occurrences" $ do
      EP.eparse (GP.repeated1 (GP.char 'a')) "aaabc" `shouldBe` GP.ParsedData ("bc", (0, 3, 3), replicate 3 'a')
      EP.eparse (GP.repeated (GP.char 'a')) "aaabc" `shouldBe` GP.ParsedData ("bc", (0, 3, 3), replicate 3 'a')
      EP.eparse (GP.repeated1 (GP.char 'a')) "xyz" `shouldBe` GP.ParserError [("yz", (0, 1, 1), "Received character: 'x'")]

    it "nat parses natural numbers" $ do
      EP.eparse GP.nat "123abc" `shouldBe` GP.ParsedData ("abc", (0, 3, 3), 123)
      EP.eparse GP.nat "abc" `shouldBe` GP.ParserError [("abc", (0, 0, 0), "Expected numeric characters"), ("bc", (0, 1, 1), "Received character: 'a'")]

    it "alternative (<|>) tries parsers in order" $ do
      EP.eparse (GP.char 'a' <|> GP.char 'b') "abc" `shouldBe` GP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse (GP.char 'a' <|> GP.char 'b') "bcd" `shouldBe` GP.ParsedData ("cd", (0, 1, 1), 'b')
      EP.eparse (GP.char 'a' <|> GP.char 'b') "xyz" `shouldBe` GP.ParserError [("yz", (0, 1, 1), "Received character: 'x'")]

    it "pairOn parses values separated by another parser" $ do
      EP.eparse (GP.pairOn (GP.char '=') (GP.repeated1 GP.letter) GP.nat) "name=42" `shouldBe` GP.ParsedData ("", (0, 7, 7), ("name", 42))

    it "sepby parses items separated by a delimiter" $ do
      EP.eparse (GP.digit // GP.char ',') "1,2,3" `shouldBe` GP.ParsedData ("", (0, 5, 5), ['1', '2', '3'])
      EP.eparse (GP.digit // GP.char ',') "1" `shouldBe` GP.ParsedData ("", (0, 1, 1), ['1'])

    it "between parses content between delimiters" $ do
      EP.eparse (GP.between (GP.char '(') (GP.char ')') (GP.repeated1 GP.letter)) "(abc)" `shouldBe` GP.ParsedData ("", (0, 5, 5), "abc")
