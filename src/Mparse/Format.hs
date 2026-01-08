module Mparse.Format where

import Control.Applicative (Alternative (empty, (<|>)))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Mparse.MParser
import Text.Read (readMaybe)

type Location = (Int, Int, Int)

data ParseLocation = ParseLocation Location deriving (Show, Eq)

instance ParseState ParseLocation where
  initialState = ParseLocation (0, 0, 0)
  consumeCharacter '\n' (ParseLocation (lcp, _, acp)) = ParseLocation (lcp + 1, 0, acp + 1)
  consumeCharacter _ (ParseLocation (lcp, rcp, acp)) = ParseLocation (lcp, rcp + 1, acp + 1)

type FStringParser = MParser ParseLocation

fstringparse :: FStringParser a -> String -> ParsedData a [String]
fstringparse = parsed

data StringComponent
  = RawString String
  | IndexedVariable Int
  | KeyedVariable String
  deriving (Show)

integer :: FStringParser Int
integer = repeated1 digit >>= parseInt
  where
    parseInt :: String -> FStringParser Int
    parseInt = maybe zero result . readMaybe

openBrace :: FStringParser Char
openBrace = sat (== '{')

closeBrace :: FStringParser Char
closeBrace = sat (== '}')

indexedVariable :: FStringParser StringComponent
indexedVariable = between open close index
  where
    open = openBrace
    close = closeBrace
    index = IndexedVariable <$> integer

stringComponents :: FStringParser [StringComponent]
stringComponents =
  do
    first <- indexedVariable <|> rawComponent
    rest <- stringComponents
    return (first : rest)
    <|> return []
  where
    escapedBraces :: FStringParser Char
    escapedBraces =
      (counted 2 openBrace >> result '{')
        <|> (counted 2 closeBrace >> result '{')
    noUnescapedBraces :: FStringParser Char
    noUnescapedBraces = sat (\c -> c /= '{' && c /= '}')
    allowedInRawString = escapedBraces <|> noUnescapedBraces
    rawComponent :: FStringParser StringComponent
    rawComponent = RawString <$> repeated1 allowedInRawString

iFmt :: String -> [String] -> String
iFmt fstring values =
  case fstringparse stringComponents fstring of
    ParsedData components -> concatMap evaluateComponent components
    _ -> fstring
  where
    valueMap :: M.Map Int String
    valueMap = M.fromList . zip [0 ..] $ values
    evaluateComponent :: StringComponent -> String
    evaluateComponent (RawString s) = s
    evaluateComponent (IndexedVariable index) = fromMaybe empty $ M.lookup index valueMap
    evaluateComponent (KeyedVariable _) = empty

kFmt :: String -> M.Map String String -> String
kFmt fstring valueMap =
  case fstringparse stringComponents fstring of
    ParsedData components -> concatMap evaluateComponent components
    _ -> fstring
  where
    evaluateComponent :: StringComponent -> String
    evaluateComponent (RawString s) = s
    evaluateComponent (IndexedVariable _) = empty
    evaluateComponent (KeyedVariable key) = fromMaybe empty $ M.lookup key valueMap
