module Mparse.Format where

import Control.Applicative (Alternative (empty, (<|>)))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Mparse.Parser
import Text.Read (readMaybe)

data StringComponent
  = RawString String
  | IndexedVariable Int
  | KeyedVariable String
  deriving (Show)

integer :: Parser Int
integer = many1 digit >>= parseInt
  where
    parseInt :: String -> Parser Int
    parseInt = maybe zero result . readMaybe

openBrace :: Parser Char
openBrace = sat (== '{')

closeBrace :: Parser Char
closeBrace = sat (== '}')

indexedVariable :: Parser StringComponent
indexedVariable = bracket open index close
  where
    open = openBrace
    close = closeBrace
    index = IndexedVariable <$> integer

stringComponents :: Parser [StringComponent]
stringComponents =
  do
    first <- indexedVariable <|> rawComponent
    rest <- stringComponents
    return (first : rest)
    <|> return []
  where
    escapedBraces :: Parser Char
    escapedBraces =
      then' const openBrace openBrace
        <|> then' const closeBrace closeBrace
    noUnescapedBraces :: Parser Char
    noUnescapedBraces = sat (\c -> c /= '{' && c /= '}')
    allowedInRawString = escapedBraces <|> noUnescapedBraces
    rawComponent :: Parser StringComponent
    rawComponent = RawString <$> many1 allowedInRawString

iFmt :: String -> [String] -> String
iFmt fstring values =
  case parsedValue stringComponents fstring of
    Nothing -> fstring
    Just components -> concatMap evaluateComponent components
  where
    valueMap :: M.Map Int String
    valueMap = M.fromList . zip [0 ..] $ values
    evaluateComponent :: StringComponent -> String
    evaluateComponent (RawString s) = s
    evaluateComponent (IndexedVariable index) = fromMaybe empty $ M.lookup index valueMap
    evaluateComponent (KeyedVariable _) = empty

kFmt :: String -> M.Map String String -> String
kFmt fstring valueMap =
  case parsedValue stringComponents fstring of
    Nothing -> fstring
    Just components -> concatMap evaluateComponent components
  where
    evaluateComponent :: StringComponent -> String
    evaluateComponent (RawString s) = s
    evaluateComponent (IndexedVariable _) = empty
    evaluateComponent (KeyedVariable key) = fromMaybe empty $ M.lookup key valueMap
