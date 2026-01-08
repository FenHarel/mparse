module Mparse.MParser where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isDigit, isLower, isUpper)

type Input = String

type ErrorMessage = String

data Expectation = None | Expectation String deriving (Show, Eq)

expectations :: (String -> String -> String) -> Expectation -> Expectation -> Expectation
expectations _ exp' None = exp'
expectations _ None exp'' = exp''
expectations f (Expectation s) (Expectation s') = Expectation $ f s s'

class ParseState a where
  initialState :: a
  consumeCharacter :: Char -> a -> a

data (ParseState s) => ParserState s = ParserState (Input, s) deriving (Show, Eq)

data (ParseState s) => ErrorChain s = Root (ErrorMessage, ParserState s) | Parent (ErrorMessage, ParserState s) (ErrorChain s) deriving (Show, Eq)

errorChain :: (ParseState s) => Expectation -> ParserState s -> ErrorChain s -> ErrorChain s
errorChain None _ = id
errorChain (Expectation exp') input = Parent (exp', input)

data (ParseState s) => ParseResult s a = Success (a, ParserState s) | Failure (ErrorChain s) deriving (Show, Eq)

succeeded :: (ParseState s) => ParseResult s a -> Bool
succeeded (Success _) = True
succeeded _ = False

someSucceeded :: (ParseState s) => [ParseResult s a] -> Bool
someSucceeded = any succeeded

type ParseFunction s a = ParserState s -> [ParseResult s a]

data (ParseState s) => MParser s a = MParser
  { expects :: Expectation,
    parse :: ParseFunction s a
  }

parser :: (ParseState s) => ParseFunction s a -> MParser s a
parser = MParser None

result :: (ParseState s) => a -> MParser s a
result x = parser (\s -> [Success (x, s)])

zero :: (ParseState s) => MParser s a
zero = parser $ const []

item :: (ParseState s) => MParser s Char
item = parser parseChar
  where
    parseChar :: (ParseState s) => ParseFunction s Char
    parseChar (ParserState ([], s)) = [Failure . Root $ ("Unexpected end of input", (ParserState ([], s)))]
    parseChar (ParserState ((x : xs), s)) = [Success (x, ParserState (xs, s'))]
      where
        s' = consumeCharacter x s

instance (ParseState s) => Functor (MParser s) where
  fmap f ep = MParser (expects ep) (fmap mapper . parse ep)
    where
      mapper (Success (a, s)) = Success $ (f a, s)
      mapper (Failure et) = Failure $ et

instance (ParseState s) => Applicative (MParser s) where
  pure = result
  ep <*> ep' = MParser exp' parse'
    where
      fexp = expects ep
      sexp = expects ep'
      exp' :: Expectation
      exp' = expectations (\s s' -> s ++ " THEN " ++ s') fexp sexp
      parse' input = do
        epr <- parse ep input
        case epr of
          (Failure et') -> pure . Failure $ errorChain fexp input et'
          (Success (f, input')) -> do
            epr' <- parse ep' input'
            case epr' of
              (Failure et') -> pure . Failure $ errorChain sexp input' et'
              (Success (a, input'')) -> pure . Success $ (f a, input'')

instance (ParseState s) => Monad (MParser s) where
  (MParser exp' parse') >>= f = MParser exp' $
    \input ->
      concat
        [ case pr of
            (Failure et) -> [Failure $ errorChain exp' input et]
            (Success (a, input')) -> parse (f a) input'
        | pr <- parse' input
        ]

instance (ParseState s) => Alternative (MParser s) where
  empty = zero
  (MParser exp' parse') <|> (MParser exp'' parse'') = MParser exp''' alternative
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
expect :: (ParseState s) => String -> MParser s a -> MParser s a
expect [] ep = ep
expect exp' (MParser _ parse') = MParser (Expectation exp') parse'

-- Consumes a character if it satisfies the given predicate
sat :: (ParseState s) => (Char -> Bool) -> MParser s Char
sat predicate = item >>= evaluate'
  where
    evaluate' :: (ParseState s) => Char -> MParser s Char
    evaluate' c'
      | predicate c' = result c'
      | otherwise = parser $ \input -> [Failure . Root $ (("Received character: '" ++ [c'] ++ "'"), input)]

-- Consumes a specific character
char :: (ParseState s) => Char -> MParser s Char
char c = sat (== c)

-- Consumes a character if it is not equal to the given character
notChar :: (ParseState s) => Char -> MParser s Char
notChar c = sat (/= c)

data Blacklist = Blacklisted | Allowed

blacklisted :: (ParseState s) => MParser s b -> MParser s Blacklist
blacklisted bp = (Blacklisted <<| bp)

(|-|) :: (ParseState s) => MParser s Blacklist -> MParser s a -> MParser s a
bp |-| ep = MParser (expects ep) parse'
  where
    parse' input' = do
      bpr' <- parse bp input'
      case bpr' of
        (Success _) -> pure . Failure . Root $ ("Blacklisted", input')
        (Failure _) -> do
          parse ep input'

-- Consumes a numeric character
digit :: (ParseState s) => MParser s Char
digit = sat isDigit

-- Consumes a lowercase letter
lower :: (ParseState s) => MParser s Char
lower = sat isLower

-- Consumes an uppercase letter
upper :: (ParseState s) => MParser s Char
upper = sat isUpper

-- Consumes an alphabetical character
letter :: (ParseState s) => MParser s Char
letter = lower <|> upper

-- Consumes am alphanumeric character
alpha :: (ParseState s) => MParser s Char
alpha = letter <|> digit

-- Consumes a specific string
exact :: (ParseState s) => String -> MParser s String
exact "" = result ""
exact s = expect exp' $ ext' s
  where
    exp' = "Expected the string: '" ++ s ++ "'"
    ext' :: (ParseState s) => String -> MParser s String
    ext' "" = result ""
    ext' (x : xs) = char x >> ext' xs >> result (x : xs)

-- Parser combinator primitives

-- Applies two parsers and combines their results with a combinator function
combine :: (ParseState s) => (a -> b -> c) -> MParser s a -> MParser s b -> MParser s c
combine f ep ep' = do
  x <- ep
  xs <- ep'
  pure $ f x xs

(|:) :: (ParseState s) => MParser s a -> MParser s [a] -> MParser s [a]
ep |: ep' = combine (:) ep ep'

(|::) :: (ParseState s) => MParser s a -> (a -> b -> c) -> (MParser s b -> MParser s c)
ep |:: f = combine f ep

(|:|) :: (ParseState s) => MParser s a -> MParser s b -> MParser s (a, b)
ep |:| ep' = combine (,) ep ep'

-- Captures `0..n` `a` values and collects them in a list
repeated :: (ParseState s) => MParser s a -> MParser s [a]
repeated ep = (ep |: (repeated ep)) <|> result []

-- Captures `0..n` `a` values and collects them in a list
repeated1 :: (ParseState s) => MParser s a -> MParser s [a]
repeated1 ep = (ep |: (repeated ep))

-- Captures `1..n` `a` values separated by b values
sepBy :: (ParseState s) => MParser s a -> MParser s b -> MParser s [a]
sepBy ep separator = ep |: (repeated (separator >> ep))

(//) :: (ParseState s) => MParser s a -> MParser s b -> MParser s [a]
ep // ep' = sepBy ep ep'

counted :: (ParseState s) => Int -> MParser s a -> MParser s [a]
counted 0 _ = result []
counted n ep = ep |: counted (n - 1) ep

-- Captures a `b` and `c` value separated by an `a` value and returns (`b`, `c`)
pairOn :: (ParseState s) => MParser s a -> MParser s b -> MParser s c -> MParser s (b, c)
pairOn separator ep = (ep <* separator) |:: (,)

-- Attempts to capture an `a` value otherwise returns a default
defaults :: (ParseState s) => a -> MParser s a -> MParser s a
defaults d ep = ep <|> result d

-- Attempts to capture an `a` value otherwise returns Nothing
optional :: (ParseState s) => MParser s a -> MParser s (Maybe a)
optional ep = (Just <$> ep) <|> result Nothing

-- Captures a `c` value that is bracketed by an `a` value and a `b` value
between :: (ParseState s) => MParser s a -> MParser s b -> MParser s c -> MParser s c
between open close ep = open >> ep <* close

parenthesized :: (ParseState s) => MParser s a -> MParser s a
parenthesized = between (char '(') (char ')')

braced :: (ParseState s) => MParser s a -> MParser s a
braced = between (char '{') (char '}')

bracketed :: (ParseState s) => MParser s a -> MParser s a
bracketed = between (char '[') (char ']')

dQuoted :: (ParseState s) => MParser s a -> MParser s a
dQuoted = between (char '"') (char '"')

sQuoted :: (ParseState s) => MParser s a -> MParser s a
sQuoted = between (char '\'') (char '\'')

-- Captures a positive integer of any length
nat :: (ParseState s) => MParser s Int
nat = expect exp' $ read <$> repeated1 digit
  where
    exp' = "Expected numeric characters"

space :: (ParseState s) => MParser s Char
space = char ' '

tab :: (ParseState s) => MParser s Char
tab = char '\t'

spaces :: (ParseState s) => MParser s String
spaces = repeated (space <|> tab)

crNL :: (ParseState s) => MParser s String
crNL = exact "\r\n"

newLine :: (ParseState s) => MParser s Char
newLine = char '\n'

notNewLine :: (ParseState s) => MParser s Char
notNewLine = notChar '\n'

spacesNL :: (ParseState s) => MParser s String
spacesNL = repeated (space <|> tab <|> newLine)

token :: (ParseState s) => MParser s a -> MParser s a
token ep = ep <* spaces

becomes :: (ParseState s) => MParser s a -> b -> MParser s b
ep `becomes` b = const b <$> ep

(|>>) :: (ParseState s) => MParser s a -> b -> MParser s b
ep |>> b = ep `becomes` b

(<<|) :: (ParseState s) => b -> MParser s a -> MParser s b
b <<| ep = ep |>> b

data ParsedData d e = ParsedData d | NoData | ParserError e deriving (Show, Eq)

type RichParsedData a s = ParsedData (Input, s, a) [(Input, s, ErrorMessage)]

_results' :: (ParseState s) => MParser s a -> String -> [ParseResult s a]
_results' (MParser exp' parse') input = map trace'' . parse' $ zeroState
  where
    zeroState = ParserState (input, initialState)
    trace'' (Failure et) = Failure $ errorChain exp' zeroState et
    trace'' s = s

parsedResults :: (ParseState s) => MParser s a -> String -> RichParsedData a s
parsedResults ep input = case _results' ep input of
  [] -> NoData
  ((Success (a, ParserState (input', s))) : _) -> ParsedData (input', s, a)
  ((Failure et : _)) -> ParserError . simplify' $ et
    where
      simplify' (Root (msg, ParserState (input', s))) = [(input', s, msg)]
      simplify' (Parent (msg, ParserState (input', s)) et') = (input', s, msg) : simplify' et'

type SuccessFormatFunction s a d = (Input, s, a) -> d

type ErrorFormatFunction s e = [(Input, s, ErrorMessage)] -> e

parsedWithFormat ::
  (ParseState s) =>
  (SuccessFormatFunction s a d) ->
  (ErrorFormatFunction s e) ->
  MParser s a ->
  String ->
  ParsedData d e
parsedWithFormat sf ef ep input = case parsedResults ep input of
  NoData -> NoData
  ParsedData d -> ParsedData . sf $ d
  ParserError e -> ParserError . ef $ e

parsed :: (ParseState s) => MParser s a -> String -> ParsedData a [ErrorMessage]
parsed = parsedWithFormat onlyData onlyError
  where
    onlyError :: ErrorFormatFunction s [ErrorMessage]
    onlyError = map (\(_, _, xs) -> xs)
    onlyData :: SuccessFormatFunction s a a
    onlyData (_, _, a) = a

parsedWithErrors :: (ParseState s) => MParser s a -> String -> ParsedData a [(Input, s, ErrorMessage)]
parsedWithErrors = parsedWithFormat onlyData id
  where
    onlyData :: SuccessFormatFunction s a a
    onlyData (_, _, a) = a

parsedDebug :: (ParseState s) => MParser s a -> String -> RichParsedData a s
parsedDebug = parsedWithFormat id id
