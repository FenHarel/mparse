module Mparse.EParser where

import Mparse.GeneralParser

type Location = (Int, Int, Int)

data ParseLocation = ParseLocation Location deriving (Show, Eq)

instance ParseState ParseLocation where
  initialState = ParseLocation (0, 0, 0)
  consumeCharacter '\n' (ParseLocation (lcp, _, acp)) = ParseLocation (lcp + 1, 0, acp + 1)
  consumeCharacter _ (ParseLocation (lcp, rcp, acp)) = ParseLocation (lcp, rcp + 1, acp + 1)

type EPParser = GeneralParser ParseLocation

eparse :: EPParser a -> String -> RichParsedData a Location
eparse = parsedWithFormat onlyData onlyError
  where
    onlyData :: (Input, ParseLocation, a) -> (Input, Location, a)
    onlyData (input, ParseLocation l, a) = (input, l, a)
    onlyError = map (\(input, ParseLocation l, e) -> (input, l, e))

-- normalizeTrace :: String -> ParserErrorTrace -> [String]
-- normalizeTrace _ = addToTrace 0
--   where
--     iFactor :: Int
--     iFactor = 1
--     bFactor :: Int
--     bFactor = 1
--     vLine :: Char
--     vLine = '|'
--     hLine :: Char
--     hLine = '-'
--     indent :: Int -> String
--     indent n = take n $ repeat ' '
--     indented :: Int -> Char -> String
--     indented 0 c = [c]
--     indented n c = indent ((3 * n) + (n * iFactor)) ++ [c]
--     padding :: Int -> String
--     padding a = (take (a * iFactor) (repeat hLine)) ++ "> "
--     header :: Int -> String
--     header 0 = ""
--     header n = (indented (n - 1) vLine) ++ padding bFactor
--     addToTrace :: Int -> ParserErrorTrace -> [String]
--     addToTrace _ (RootError (msg, p)) = (msg ++ " at position " ++ show p) : []
--     addToTrace pad (ParentError (msg, p) (RootError (msg', p'))) =
--       (header pad ++ msg ++ " at position " ++ show p)
--         : ((indented pad vLine) ++ padding bFactor ++ msg' ++ " at position " ++ show p')
--         : []
--     addToTrace pad (ParentError (msg, p) et') =
--       (header pad ++ msg ++ " at position " ++ show p)
--         : addToTrace (pad + 1) et'

-- printErrorTrace :: ParserErrorTrace -> IO [()]
-- printErrorTrace = mapM print . normalizeTrace ""
