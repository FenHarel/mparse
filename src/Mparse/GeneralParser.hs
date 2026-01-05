module Mparse.GeneralParser where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isDigit, isLower, isUpper)

type Input = String

data Expectation = None | Expectation String deriving (Show, Eq)

expectations :: (String -> String -> String) -> Expectation -> Expectation -> Expectation
expectations _ exp' None = exp'
expectations _ None exp'' = exp''
expectations f (Expectation s) (Expectation s') = Expectation $ f s s'

class ParsingState a where
  initialState :: a
  consumeCharacter :: Char -> a -> a

class FailState a where
  unexpectedEndOfInput :: (ParsingState s) => (String, s) -> Maybe a
  traceError :: (ParsingState s) => Expectation -> (String, s) -> a -> a
  rootError :: (String, s) -> a

data (ParsingState s, FailState f) => GeneralParsingResult s f a = Success (a, (String, s)) | Failure f

succeeded :: (ParsingState s, FailState f) => GeneralParsingResult s f a -> Bool
succeeded (Success _) = True
succeeded _ = False

someSucceeded :: (ParsingState s, FailState f) => [GeneralParsingResult s f a] -> Bool
someSucceeded = any succeeded

type GeneralParsingFunction s f a = (Input, s) -> [GeneralParsingResult s f a]

data (ParsingState s, FailState f) => GeneralParser s f a = GeneralParser
  { expects :: Expectation,
    parse :: GeneralParsingFunction s f a
  }

parser :: (ParsingState s, FailState f) => GeneralParsingFunction s f a -> GeneralParser s f a
parser = GeneralParser None

result :: (ParsingState s, FailState f) => x -> GeneralParser s f x
result x = parser (\s -> [Success (x, s)])

zero :: (ParsingState s, FailState f) => GeneralParser s f a
zero = parser $ const []

item :: (ParsingState s, FailState f) => GeneralParser s f Char
item = parser parseChar
  where
    parseChar :: (ParsingState s, FailState f) => GeneralParsingFunction s f Char
    parseChar ([], s) = case unexpectedEndOfInput ([], s) of
      Just f -> [Failure f]
      Nothing -> []
    parseChar ((x : xs), s) = [Success (x, (xs, s'))]
      where
        s' = consumeCharacter x s

instance (ParsingState s, FailState f) => Functor (GeneralParser s f) where
  fmap f ep = GeneralParser (expects ep) (fmap mapper . parse ep)
    where
      mapper (Success (a, s)) = Success $ (f a, s)
      mapper (Failure et) = Failure $ et

instance (ParsingState s, FailState f) => Applicative (GeneralParser s f) where
  pure = result
  ep <*> ep' = GeneralParser exp' parse'
    where
      fexp = expects ep
      sexp = expects ep'
      exp' :: Expectation
      exp' = expectations (\s s' -> s ++ " THEN " ++ s') fexp sexp
      parse' input = do
        epr <- parse ep input
        case epr of
          (Failure et') -> pure $ Failure $ traceError fexp input et'
          (Success (f, input')) -> do
            epr' <- parse ep' input'
            case epr' of
              (Failure et') -> pure $ Failure $ traceError sexp input' et'
              (Success (a, input'')) -> pure $ Success (f a, input'')

instance (ParsingState s, FailState f) => Monad (GeneralParser s f) where
  (GeneralParser exp' parse') >>= f = GeneralParser exp' $
    \input ->
      concat
        [ case pr of
            (Failure et) -> [Failure $ traceError exp' input et]
            (Success (a, input')) -> parse (f a) input'
        | pr <- parse' input
        ]

instance (ParsingState s, FailState f) => Alternative (GeneralParser s f) where
  empty = zero
  (GeneralParser exp' parse') <|> (GeneralParser exp'' parse'') = GeneralParser exp''' alternative
    where
      exp''' :: Expectation
      exp''' = expectations (\s s' -> s ++ " OR " ++ s') exp' exp''
      alternative input
        | someSucceeded firstParse = firstParse
        | otherwise = secondParse
        where
          firstParse = parse' input
          secondParse = parse'' input

-- Adds an expectation to a parser
expect :: (ParsingState s, FailState f) => String -> GeneralParser s f a -> GeneralParser s f a
expect [] ep = ep
expect exp' (GeneralParser _ parse') = (GeneralParser (Expectation exp') parse')

-- Consumes a character if it satisfies the given predicate
sat :: (ParsingState s, FailState f) => (Char -> Bool) -> GeneralParser s f Char
sat predicate = item >>= evaluate'
  where
    evaluate' :: (ParsingState s, FailState f) => Char -> GeneralParser s f Char
    evaluate' c'
      | predicate c' = result c'
      | otherwise = parser $ \(_, p) -> [Failure (rootError (("Received character: '" ++ [c'] ++ "'"), p))]

-- Consumes a specific character
char :: (ParsingState s, FailState f) => Char -> GeneralParser s f Char
char c = sat (== c)

-- Consumes a character if it is not equal to the given character
notChar :: (ParsingState s, FailState f) => Char -> GeneralParser s f Char
notChar c = sat (/= c)

-- Consumes a numeric character
digit :: (ParsingState s, FailState f) => GeneralParser s f Char
digit = sat isDigit

-- Consumes a lowercase letter
lower :: (ParsingState s, FailState f) => GeneralParser s f Char
lower = sat isLower

-- Consumes an uppercase letter
upper :: (ParsingState s, FailState f) => GeneralParser s f Char
upper = sat isUpper

-- Consumes an alphabetical character
letter :: (ParsingState s, FailState f) => GeneralParser s f Char
letter = lower <|> upper

-- Consumes am alphanumeric character
alpha :: (ParsingState s, FailState f) => GeneralParser s f Char
alpha = letter <|> digit

-- Consumes a specific string
exact :: (ParsingState s, FailState f) => String -> GeneralParser s f String
exact "" = result ""
exact s = expect exp' $ ext' s
  where
    exp' = "Expected the string: '" ++ s ++ "'"
    ext' :: (ParsingState s, FailState f) => String -> GeneralParser s f String
    ext' "" = result ""
    ext' (x : xs) = char x >> ext' xs >> result (x : xs)

-- Parser combinator primitives

-- Applies two parsers and combines their results with a combinator function
combine :: (ParsingState s, FailState f) => (a -> b -> c) -> GeneralParser s f a -> GeneralParser s f b -> GeneralParser s f c
combine f ep ep' = do
  x <- ep
  xs <- ep'
  pure $ f x xs

(|:) :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f [a] -> GeneralParser s f [a]
ep |: ep' = combine (:) ep ep'

(|::) :: (ParsingState s, FailState f) => GeneralParser s f a -> (a -> b -> c) -> (GeneralParser s f b -> GeneralParser s f c)
ep |:: f = combine f ep

(|:|) :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f b -> GeneralParser s f (a, b)
ep |:| ep' = combine (,) ep ep'

-- Captures `0..n` `a` values and collects them in a list
repeated :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f [a]
repeated ep = (ep |: (repeated ep)) <|> result []

-- Captures `0..n` `a` values and collects them in a list
repeated1 :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f [a]
repeated1 ep = (ep |: (repeated ep))

-- Captures `1..n` `a` values separated by b values
sepBy :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f b -> GeneralParser s f [a]
sepBy ep separator = ep |: (repeated (separator >> ep))

(//) :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f b -> GeneralParser s f [a]
ep // ep' = sepBy ep ep'

counted :: (ParsingState s, FailState f) => Int -> GeneralParser s f a -> GeneralParser s f [a]
counted 0 _ = result []
counted n ep = ep |: counted (n - 1) ep

-- Captures a `b` and `c` value separated by an `a` value and returns (`b`, `c`)
pairOn :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f b -> GeneralParser s f c -> GeneralParser s f (b, c)
pairOn separator ep = (ep <* separator) |:: (,)

-- Attempts to capture an `a` value otherwise returns a default
defaults :: (ParsingState s, FailState f) => a -> GeneralParser s f a -> GeneralParser s f a
defaults d ep = ep <|> result d

-- Attempts to capture an `a` value otherwise returns Nothing
optional :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f (Maybe a)
optional ep = (Just <$> ep) <|> result Nothing

-- Captures a `c` value that is bracketed by an `a` value and a `b` value
between :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f b -> GeneralParser s f c -> GeneralParser s f c
between open close ep = open >> ep <* close

parenthesized :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f a
parenthesized = between (char '(') (char ')')

braced :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f a
braced = between (char '{') (char '}')

bracketed :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f a
bracketed = between (char '[') (char ']')

dQuoted :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f a
dQuoted = between (char '"') (char '"')

sQuoted :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f a
sQuoted = between (char '\'') (char '\'')

-- Captures a positive integer of any length
nat :: (ParsingState s, FailState f) => GeneralParser s f Int
nat = expect exp' $ read <$> repeated1 digit
  where
    exp' = "Expected numeric characters"

space :: (ParsingState s, FailState f) => GeneralParser s f Char
space = char ' '

tab :: (ParsingState s, FailState f) => GeneralParser s f Char
tab = char '\t'

spaces :: (ParsingState s, FailState f) => GeneralParser s f String
spaces = repeated (space <|> tab)

crNL :: (ParsingState s, FailState f) => GeneralParser s f String
crNL = exact "\r\n"

newLine :: (ParsingState s, FailState f) => GeneralParser s f Char
newLine = char '\n'

notNewLine :: (ParsingState s, FailState f) => GeneralParser s f Char
notNewLine = notChar '\n'

spacesNL :: (ParsingState s, FailState f) => GeneralParser s f String
spacesNL = repeated (space <|> tab <|> newLine)

token :: (ParsingState s, FailState f) => GeneralParser s f a -> GeneralParser s f a
token ep = ep <* spaces

parseResults :: (ParsingState s, FailState f) => GeneralParser s f a -> String -> [GeneralParsingResult s f a]
parseResults (GeneralParser exp' parse') input = map trace'' . parse' $ zeroState
  where
    zeroState = (input, initialState)
    trace'' :: (ParsingState s, FailState f) => GeneralParsingResult s f a -> GeneralParsingResult s f a
    trace'' (Failure et) = Failure $ traceError exp' zeroState et
    trace'' s = s

data (FailState f) => ParsedData a f = ParsedData a | NoData | ParserError f deriving (Show, Eq)

parsed :: (ParsingState s, FailState f) => GeneralParser s f a -> String -> ParsedData a f
parsed ep input = case parseResults ep input of
  [] -> NoData
  ((Success (a, _)) : _) -> ParsedData a
  ((Failure et) : _) -> ParserError et
