module Mparse.EParser where

import Mparse.MParser

type Location = (Int, Int, Int)

data ParseLocation = ParseLocation Location deriving (Show, Eq)

instance ParseState ParseLocation where
  initialState = ParseLocation (0, 0, 0)
  consumeCharacter '\n' (ParseLocation (lcp, _, acp)) = ParseLocation (lcp + 1, 0, acp + 1)
  consumeCharacter _ (ParseLocation (lcp, rcp, acp)) = ParseLocation (lcp, rcp + 1, acp + 1)

type EPParser = MParser ParseLocation

eparse :: EPParser a -> String -> RichParsedData a Location
eparse = parsedWithFormat onlyData onlyError
  where
    onlyData :: (Input, ParseLocation, a) -> (Input, Location, a)
    onlyData (input, ParseLocation l, a) = (input, l, a)
    onlyError = map (\(input, ParseLocation l, e) -> (input, l, e))
