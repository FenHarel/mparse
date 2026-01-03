module Mparse.EParser where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)

type Position = Int

type InputPosition = (String, Position)

data ParserErrorTrace = RootError String Position | ParentError String Position ParserErrorTrace deriving (Show, Eq)

instance Ord ParserErrorTrace where
  compare = on compare etp'
    where
      etp' (RootError _ p) = p
      etp' (ParentError _ p _) = p

data Expectation = None | Expectation String deriving (Show, Eq)

expectations :: (String -> String -> String) -> Expectation -> Expectation -> Expectation
expectations _ exp' None = exp'
expectations _ None exp'' = exp''
expectations f (Expectation s) (Expectation s') = Expectation $ f s s'

errTrace :: Expectation -> Position -> ParserErrorTrace -> ParserErrorTrace
errTrace None _ = id
errTrace (Expectation exp') p = ParentError exp' p

data ParseResult a = Success (a, String, Position) | Failure ParserErrorTrace deriving (Show, Eq)

succeeded :: ParseResult a -> Bool
succeeded (Success _) = True
succeeded _ = False

someSucceeded :: [ParseResult a] -> Bool
someSucceeded = any succeeded

data EParser a = EParser
  { expects :: Expectation,
    parse :: (String, Position) -> [ParseResult a]
  }

instance Functor EParser where
  fmap f ep = EParser (expects ep) (fmap mapper . parse ep)
    where
      mapper (Success (a, input, p)) = Success $ (f a, input, p)
      mapper (Failure et) = Failure $ et

instance Applicative EParser where
  pure = result
  ep <*> ep' = EParser exp' parse'
    where
      fexp = expects ep
      sexp = expects ep'
      exp' :: Expectation
      exp' = expectations (\s s' -> s ++ " THEN " ++ s') fexp sexp
      parse' (input, p) = do
        epr <- parse ep (input, p)
        case epr of
          (Failure et') -> pure $ Failure $ errTrace fexp p et'
          (Success (f, input', p')) -> do
            epr' <- parse ep' (input', p')
            case epr' of
              (Failure et') -> pure $ Failure $ errTrace sexp p' et'
              (Success (a, input'', p'')) -> pure $ Success ((f a), input'', p'')

instance Monad EParser where
  ep >>= f = EParser exp' $
    \(input, p) ->
      concat
        [ case pr of
            (Failure et) -> [Failure $ errTrace exp' p et]
            (Success (a, input', p')) -> parse (f a) (input', p')
        | pr <- parse ep (input, p)
        ]
    where
      exp' = expects ep

instance Alternative EParser where
  empty = zero
  ep <|> ep' = EParser exp' alternative
    where
      exp' :: Expectation
      exp' = on (expectations (\s s' -> s ++ " OR " ++ s')) expects ep ep'
      alternative (input, p)
        | someSucceeded firstParse = firstParse
        | otherwise = secondParse
        where
          firstParse = parse ep (input, p)
          secondParse = parse ep' (input, p)

-- Parser constructor primitives
--
-- Constructs a Parser with no expectation
parser :: ((String, Position) -> [ParseResult a]) -> EParser a
parser = EParser None

-- Adds an expectation to a parser
expect :: String -> EParser a -> EParser a
expect [] ep = ep
expect exp' (EParser _ parse') = (EParser (Expectation exp') parse')

-- pure for the EParser monad
result :: a -> EParser a
result a = parser (\(input, p) -> [Success (a, input, p)])

-- empty for the EParser applicative
zero :: EParser a
zero = parser $ const []

-- Parser primitives
--
-- Consumes a character
item :: EParser Char
item = parser parseChar
  where
    parseChar ([], p) = [Failure (RootError "Unexpected end of input." p)]
    parseChar ((x : xs), p) = [Success (x, xs, p + 1)]

-- Consumes a character if it satisfies the given predicate
sat :: (Char -> Bool) -> EParser Char
sat predicate = item >>= evaluate'
  where
    evaluate' :: Char -> EParser Char
    evaluate' c'
      | predicate c' = result c'
      | otherwise = parser $ \(_, p) -> [Failure (RootError ("Received character: '" ++ [c'] ++ "'") p)]

-- Consumes a specific character
char :: Char -> EParser Char
char c = sat (== c)

-- Consumes a character if it is not equal to the given character
notChar :: Char -> EParser Char
notChar c = sat (/= c)

-- Consumes a numeric character
digit :: EParser Char
digit = sat isDigit

-- Consumes a lowercase letter
lower :: EParser Char
lower = sat isLower

-- Consumes an uppercase letter
upper :: EParser Char
upper = sat isUpper

-- Consumes an alphabetical character
letter :: EParser Char
letter = lower <|> upper

-- Consumes am alphanumeric character
alpha :: EParser Char
alpha = letter <|> digit

-- Consumes a specific string
exact :: String -> EParser String
exact "" = result ""
exact s = expect exp' $ ext' s
  where
    exp' = "Expected the string: '" ++ s ++ "'"
    ext' :: String -> EParser String
    ext' "" = result ""
    ext' (x : xs) = char x >> ext' xs >> result (x : xs)

-- Parser combinator primitives

-- Applies two parsers and combines their results with a combinator function
combine :: (a -> b -> c) -> EParser a -> EParser b -> EParser c
combine f ep ep' = do
  x <- ep
  xs <- ep'
  pure $ f x xs

(|:) :: EParser a -> EParser [a] -> EParser [a]
ep |: ep' = combine (:) ep ep'

(|::) :: EParser a -> (a -> b -> c) -> (EParser b -> EParser c)
ep |:: f = combine f ep

-- Captures `0..n` `a` values and collects them in a list
repeated :: EParser a -> EParser [a]
repeated ep = (ep |: (repeated ep)) <|> result []

-- Captures `0..n` `a` values and collects them in a list
repeated1 :: EParser a -> EParser [a]
repeated1 ep = (ep |: (repeated ep))

-- Captures `1..n` `a` values separated by b values
sepBy :: EParser a -> EParser b -> EParser [a]
sepBy ep separator = ep |: (repeated (separator >> ep))

(//) :: EParser a -> EParser b -> EParser [a]
ep // ep' = sepBy ep ep'

-- Captures a `b` and `c` value separated by an `a` value and returns (`b`, `c`)
between :: EParser a -> EParser b -> EParser c -> EParser (b, c)
between separator ep = (ep <* separator) |:: (,)

-- Attempts to capture an `a` value otherwise returns a default
defaults :: a -> EParser a -> EParser a
defaults d ep = ep <|> result d

-- Attempts to capture an `a` value otherwise returns Nothing
optional :: EParser a -> EParser (Maybe a)
optional ep = (Just <$> ep) <|> result Nothing

-- Captures a `c` value that is bracketed by an `a` value and a `b` value
bracket :: EParser a -> EParser b -> EParser c -> EParser c
bracket open close ep = open >> ep <* close

-- Captures a positive integer of any length
nat :: EParser Int
nat = expect exp' $ read <$> repeated1 digit
  where
    exp' = "Expected numeric characters"

space :: EParser Char
space = char ' '

tab :: EParser Char
tab = char '\t'

spaces :: EParser String
spaces = repeated (space <|> tab)

nl :: EParser Char
nl = char '\n'

crNL :: EParser String
crNL = exact "\r\n"

newLine :: EParser Char
newLine = nl <|> (crNL >> result '\n')

notNewLine :: EParser Char
notNewLine = notChar '\n'

spacesNL :: EParser String
spacesNL = repeated (space <|> tab <|> newLine)

token :: EParser a -> EParser a
token ep = ep <* spaces

uparse :: EParser a -> String -> [ParseResult a]
uparse ep input = map enhance $ parse ep (input, 0)
  where
    enhance :: ParseResult a -> ParseResult a
    enhance (Failure et) = Failure $ errTrace (expects ep) 0 et
    enhance s = s

data ParsedData a = ParsedData a | NoData | ParserError ParserErrorTrace deriving (Show, Eq)

parsed :: EParser a -> String -> ParsedData a
parsed ep input = case parse ep (input, 0) of
  [] -> NoData
  ((Success (a, _, _)) : _) -> ParsedData a
  ((Failure et) : _) -> ParserError . errTrace (expects ep) 0 $ et

parseForced :: EParser a -> String -> a
parseForced ep input = case parse ep (input, 0) of
  [] -> error "No parsed data"
  ((Failure (RootError msg p)) : _) -> error $ "Failed to parse: " ++ msg ++ " at position " ++ show p
  ((Failure (ParentError msg p _)) : _) -> error $ "Failed to parse: " ++ msg ++ " at position " ++ show p ++ " and more..."
  ((Success (a, _, _)) : _) -> a

normalizeTrace :: String -> ParserErrorTrace -> [String]
normalizeTrace _ = addToTrace 0
  where
    iFactor :: Int
    iFactor = 1
    bFactor :: Int
    bFactor = 1
    vLine :: Char
    vLine = '|'
    hLine :: Char
    hLine = '-'
    indent :: Int -> String
    indent n = take n $ repeat ' '
    indented :: Int -> Char -> String
    indented 0 c = [c]
    indented n c = indent ((3 * n) + (n * iFactor)) ++ [c]
    padding :: Int -> String
    padding a = (take (a * iFactor) (repeat hLine)) ++ "> "
    header :: Int -> String
    header 0 = ""
    header n = (indented (n - 1) vLine) ++ padding bFactor
    addToTrace :: Int -> ParserErrorTrace -> [String]
    addToTrace _ (RootError msg p) = (msg ++ " at position " ++ show p) : []
    addToTrace pad (ParentError msg p (RootError msg' p')) =
      (header pad ++ msg ++ " at position " ++ show p)
        : ((indented pad vLine) ++ padding bFactor ++ msg' ++ " at position " ++ show p')
        : []
    addToTrace pad (ParentError msg p et') =
      (header pad ++ msg ++ " at position " ++ show p)
        : addToTrace (pad + 1) et'

printErrorTrace :: ParserErrorTrace -> IO [()]
printErrorTrace = mapM print . normalizeTrace ""
