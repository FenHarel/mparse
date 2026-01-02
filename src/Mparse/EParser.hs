module Mparse.EParser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)

type Position = Int

type InputPosition = (String, Position)

data Expectation = None | Expectation String deriving (Show, Eq)

expect :: String -> Expectation
expect = Expectation

data ParserErrorTrace = RootError String Position | ParentError String Position ParserErrorTrace deriving (Show, Eq)

instance Ord ParserErrorTrace where
  compare = on compare etp'
    where
      etp' (RootError _ p) = p
      etp' (ParentError _ p _) = p

errTrace :: Expectation -> Position -> ParserErrorTrace -> ParserErrorTrace
errTrace None _ = id
errTrace (Expectation exp') p = ParentError exp' p

data ParseResult a = Success (a, String, Position) | Failure ParserErrorTrace deriving (Show, Eq)

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
  pure = result' None
  ep <*> ep' = EParser (expT (expects ep) (expects ep')) $ \(input, p) -> do
    Success (f, input', p') <- parse ep (input, p)
    Success (a, input'', p'') <- parse ep' (input', p')
    return $ Success (f a, input'', p'')
    where
      expT :: Expectation -> Expectation -> Expectation
      expT None None = None
      expT exp' None = exp'
      expT None exp'' = exp''
      expT (Expectation exp') (Expectation exp'') = Expectation $ exp' ++ " THEN " ++ exp''

accumulateResults :: ([ParseResult a], Maybe ParserErrorTrace) -> [ParseResult a] -> Either [ParseResult a] ParserErrorTrace
accumulateResults ([], Just e) [] = Right e
accumulateResults (ss, _) [] = Left ss
accumulateResults (ss, Nothing) (pr : prs) = case pr of
  (Failure e') -> accumulateResults (ss, Just e') prs
  success -> accumulateResults (success : ss, Nothing) prs
accumulateResults (ss, Just e) (pr : prs) = case pr of
  (Failure e') -> accumulateResults (ss, Just (max e e')) prs
  success -> accumulateResults (success : ss, Just e) prs

evaluated' :: EParser a -> (String, Position) -> Either [ParseResult a] ParserErrorTrace
evaluated' ep (input, p) = case accumulateResults ([], Nothing) $ parse ep (input, p) of
  Right et -> Right $ errTrace (expects ep) p et
  s -> s

reduced' :: [ParseResult a] -> [ParseResult a]
reduced' results = case r' of
  Left successes -> successes
  Right et -> [Failure et]
  where
    r' = accumulateResults ([], Nothing) results

rparse :: EParser a -> (String, Position) -> [ParseResult a]
rparse ep (input, p) = reduced' $ parse ep (input, p)

instance Monad EParser where
  ep >>= f = EParser exp' $
    \(input, p) -> case evaluated' ep (input, p) of
      Left successes -> concatMap reduced' [rparse (f value) (input', p') | (Success (value, input', p')) <- successes]
      Right failure -> [Failure $ errTrace (expects ep) p failure]
    where
      exp' = expects ep

instance MonadPlus EParser where
  mzero = zero
  mplus ep ep' = EParser (exps' (expects ep) (expects ep')) $
    \(input, p) -> reduced' $ parse ep (input, p) ++ parse ep' (input, p)
    where
      exps' :: Expectation -> Expectation -> Expectation
      exps' None None = None
      exps' ex None = ex
      exps' None ex' = ex'
      exps' (Expectation ex) (Expectation ex') = Expectation $ ex ++ " OR " ++ ex'

instance Alternative EParser where
  empty = zero
  ep <|> ep' = EParser (expects ep'') $
    \(input, p) -> case parse ep'' (input, p) of
      [] -> []
      (x : _) -> [x]
    where
      ep'' = mplus ep ep'

expo :: Expectation -> EParser a -> EParser a
expo exp' (EParser _ parse') = EParser exp' parse'

parser :: ((String, Position) -> [ParseResult a]) -> EParser a
parser = EParser None

eparser :: Expectation -> ((String, Position) -> [ParseResult a]) -> EParser a
eparser exp' = expo exp' . parser

err :: String -> EParser a
err msg = parser $ \(_, p) -> [Failure (RootError msg p)]

err' :: Expectation -> String -> EParser a
err' exp' = expo exp' . err

result :: a -> EParser a
result a = parser (\(input, p) -> [Success (a, input, p)])

result' :: Expectation -> a -> EParser a
result' exp' = expo exp' . result

rootError :: String -> ((String, Position) -> ParseResult a)
rootError msg = \(_, p) -> Failure (RootError msg p)

zero :: EParser a
zero = parser $ const []

item :: EParser Char
item = parser parseChar
  where
    parseChar ([], p) = [Failure (RootError "Unexpected end of input." p)]
    parseChar ((x : xs), p) = [Success (x, xs, p + 1)]

item' :: Expectation -> EParser Char
item' exp' = expo exp' item

sat :: (Char -> Bool) -> EParser Char
sat predicate = item >>= evaluate'
  where
    evaluate' :: Char -> EParser Char
    evaluate' c'
      | predicate c' = result c'
      | otherwise = err $ "Received character: '" ++ [c'] ++ "'"

sat' :: Expectation -> (Char -> Bool) -> EParser Char
sat' exp' = expo exp' . sat

char :: Char -> EParser Char
char c = sat (== c)

char' :: Expectation -> Char -> EParser Char
char' exp' = expo exp' . char

digit :: EParser Char
digit = sat' exp' isDigit
  where
    exp' = expect "Expected numeric digit."

lower :: EParser Char
lower = sat' exp' isLower
  where
    exp' = expect "Expected lowercase character."

upper :: EParser Char
upper = sat' exp' isUpper
  where
    exp' = expect "Expected uppercase character."

letter :: EParser Char
letter = lower <|> upper

alpha :: EParser Char
alpha = letter <|> digit

string :: String -> EParser String
string "" = result' None ""
string s = expo exp' $ str' s
  where
    exp' = expect $ "Expected the string: '" ++ s ++ "'"
    str' :: String -> EParser String
    str' "" = result' None ""
    str' (x : xs) = char' None x >> str' xs >> result' None (x : xs)

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

normalizeTrace :: String -> ParserErrorTrace -> [String]
normalizeTrace _ = addToTrace 0
  where
    -- posIndexed = zip str [1 ..]
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
