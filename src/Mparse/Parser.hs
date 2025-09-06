module Mparse.Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..), void)
import qualified Data.Bifunctor as Bifunctor
import Data.Char (isDigit, isLower, isSpace, isUpper)

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = Parser (fmap (Bifunctor.first f) . parse p)

instance Applicative Parser where
  pure = result
  p1 <*> p2 = Parser $ \input -> do
    (f, input') <- parse p1 input
    (a, input'') <- parse p2 input'
    return (f a, input'')

instance Monad Parser where
  p >>= f = Parser $ \input -> concat [parse (f value) input' | (value, input') <- parse p input]

instance MonadPlus Parser where
  mzero = zero
  p1 `mplus` p2 = Parser $ \input -> parse p1 input ++ parse p2 input

instance Alternative Parser where
  empty = zero
  p1 <|> p2 = Parser $
    \input -> case parse (mplus p1 p2) input of
      [] -> []
      (x : _) -> [x]

result :: a -> Parser a
result str = Parser $ \input -> [(str, input)]

zero :: Parser a
zero = Parser $ const []

item :: Parser Char
item = Parser parseChar
  where
    parseChar [] = []
    parseChar (x : xs) = [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat predicate = item >>= \c -> if predicate c then result c else zero

char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = lower <|> upper

alpha :: Parser Char
alpha = letter <|> digit

string :: String -> Parser String
string "" = result ""
string (x : xs) = char x >> string xs >> result (x : xs)

many' :: Parser a -> Parser [a]
many' p =
  do
    x <- p
    xs <- many' p
    return (x : xs)
    <|> return []

many1 :: Parser a -> Parser [a]
many1 p =
  do
    x <- p
    xs <- many' p
    return (x : xs)

then' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
then' combine p q =
  do
    x <- p
    x' <- q
    return $ combine x x'

thenList :: Parser a -> Parser [a] -> Parser [a]
thenList = then' (:)

ident :: Parser String
ident = alpha_ `thenList` many' (alpha_ <|> digit)
  where
    alpha_ = letter <|> char '_'

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep =
  do
    x <- p
    xs <- many' (sep >> p)
    return (x : xs)

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = open >> p <* close

nat :: Parser Int
nat = read <$> many1 digit

spaces :: Parser ()
spaces = void $ many' (sat (\c -> c == ' ' || c == '\t'))

spacesNL :: Parser ()
spacesNL = void $ many' (sat isSpace)

newLine :: Parser Char
newLine = sat (== '\n')

notNewLine :: Parser Char
notNewLine = sat (/= '\n')

token :: Parser a -> Parser a
token p = p <* spaces

identifier :: Parser String
identifier = token ident

between :: Parser a -> Parser b -> Parser c -> Parser (b, c)
between sep p q =
  do
    b <- p
    _ <- sep
    c <- q
    return (b, c)

zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne p = (Just <$> p) <|> return Nothing

pair :: Parser Char -> Parser Char -> Parser String
pair = then' (\a b -> [a, b])

equals' :: Parser a -> Parser b -> Parser (a, b)
equals' = between (token $ sat (== '='))

parsedValue :: Parser a -> String -> Maybe a
parsedValue p s = case parse p s of
  [] -> Nothing
  ((a, _) : _) -> Just a
