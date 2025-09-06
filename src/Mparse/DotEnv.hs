module Mparse.DotEnv where

import Control.Applicative (Alternative ((<|>)))
import Mparse.Parser

data ValueComponent
  = Literal String
  | Variable String
  | Command String
  deriving (Show, Eq)

variable :: Parser ValueComponent
variable = bracket openSub var closeSub
  where
    openSub = then' (\a b -> [a, b]) (sat (== '$')) (sat (== '{'))
    closeSub = sat (== '}')
    var :: Parser ValueComponent
    var = Variable <$> many1 (Parser withEscapedClose)
    withEscapedClose [] = []
    withEscapedClose ('}' : _) = []
    withEscapedClose ('\\' : '}' : xs) = [('}', xs)]
    withEscapedClose (x : xs) = [(x, xs)]

command :: Parser ValueComponent
command = bracket openSub com closeSub
  where
    openSub = pair (sat (== '$')) (sat (== '('))
    closeSub = sat (== ')')
    com :: Parser ValueComponent
    com = Command <$> many1 (Parser withEscapedClose)
    withEscapedClose [] = []
    withEscapedClose (')' : _) = []
    withEscapedClose ('\\' : ')' : xs) = [(')', xs)]
    withEscapedClose (x : xs) = [(x, xs)]

data ComponentEvaluationMode
  = Raw
  | Debug
  | EnvLoad
  deriving (Show)

raw :: ValueComponent -> String
raw (Literal s) = s
raw (Variable s) = "${" ++ s ++ "}"
raw (Command s) = "$(" ++ s ++ ")"

debugRaw :: ValueComponent -> String
debugRaw (Literal s) = s
debugRaw (Variable s) = "<<" ++ s ++ ">>"
debugRaw (Command s) = "<" ++ s ++ ">"

data ComponentizedValue = ComponentizedValue
  { _name :: String,
    _components :: [ValueComponent]
  }
  deriving (Show)

evaluate' :: ComponentEvaluationMode -> ComponentizedValue -> (String, String)
evaluate' mode (ComponentizedValue name components) = (name, concatMap evaluateComponent components)
  where
    evaluateComponentForMode :: ComponentEvaluationMode -> ValueComponent -> String
    evaluateComponentForMode _ (Literal v) = v
    evaluateComponentForMode Raw (Variable v) = concat ["${", v, "}"]
    evaluateComponentForMode Debug (Variable v) = concat ["<<", v, ">>"]
    evaluateComponentForMode Raw (Command v) = concat ["$(", v, ")"]
    evaluateComponentForMode Debug (Command v) = concat ["<", v, ">"]
    evaluateComponentForMode EnvLoad _ = error "not implemented"
    evaluateComponent = evaluateComponentForMode mode

valueComponentsForDoubleQuotes' :: Parser [ValueComponent]
valueComponentsForDoubleQuotes' =
  do
    value <- variable <|> command <|> literalValue
    values <- if isEmptyLiteral value then return [] else valueComponentsForDoubleQuotes'
    return (value : values)
    <|> return []
  where
    literalValue = Literal <$> many' escapeItem
    isEmptyLiteral :: ValueComponent -> Bool
    isEmptyLiteral (Literal "") = True
    isEmptyLiteral _ = False
    escapeItem = Parser parseEscapeChar
    parseEscapeChar [] = []
    parseEscapeChar ('"' : _) = []
    parseEscapeChar ('$' : '(' : _) = []
    parseEscapeChar ('$' : '{' : _) = []
    parseEscapeChar ('\\' : '\n' : xs) = [('\n', xs)]
    parseEscapeChar ('\\' : '\r' : xs) = [('\r', xs)]
    parseEscapeChar ('\\' : '\t' : xs) = [('\t', xs)]
    parseEscapeChar ('\\' : '\f' : xs) = [('\f', xs)]
    parseEscapeChar ('\\' : '\b' : xs) = [('\b', xs)]
    parseEscapeChar ('\\' : '\"' : xs) = [('\"', xs)]
    parseEscapeChar ('\\' : '\'' : xs) = [('\'', xs)]
    parseEscapeChar ('\\' : '\\' : xs) = [('\\', xs)]
    parseEscapeChar ('\\' : '$' : xs) = [('$', xs)]
    parseEscapeChar ('\\' : x : xs) = [(x, xs)]
    parseEscapeChar (x : xs) = [(x, xs)]

unquotedVariable :: Parser ComponentizedValue
unquotedVariable =
  do
    (name, components) <- identifier `equals'` valueComponents
    return (ComponentizedValue name components)
  where
    valueComponents :: Parser [ValueComponent]
    valueComponents =
      do
        value <- command <|> variable <|> literalValue
        values <- valueComponents
        return (value : values)
        <|> return []
      where
        literalValue = Literal <$> many1 literal
        literal = Parser parseLiteral
        parseLiteral [] = []
        parseLiteral (' ' : _) = []
        parseLiteral ('\t' : _) = []
        parseLiteral ('#' : _) = []
        parseLiteral ('\n' : _) = []
        parseLiteral ('$' : '(' : _) = []
        parseLiteral ('$' : '{' : _) = []
        parseLiteral ('\\' : '$' : xs) = [('$', xs)]
        parseLiteral (x : xs) = [(x, xs)]

variableInSingleQuotes :: Parser ComponentizedValue
variableInSingleQuotes =
  do
    (name, value) <- identifier `equals'` valueInSingleQuotes
    return (ComponentizedValue name [Literal value])
  where
    sq = char '\''
    notSq = sat (/= '\'')
    valueInSingleQuotes = token $ bracket sq (many' notSq) sq

variableInDoubleQuotes :: Parser ComponentizedValue
variableInDoubleQuotes =
  do
    (name, results) <- identifier `equals'` valueComponents
    return (ComponentizedValue name results)
  where
    valueComponents :: Parser [ValueComponent]
    valueComponents = token $ bracket (char '"') valueComponentsForDoubleQuotes (char '"')
    valueComponentsForDoubleQuotes :: Parser [ValueComponent]
    valueComponentsForDoubleQuotes =
      do
        value <- variable <|> command <|> literalValue
        values <- if isEmptyLiteral value then return [] else valueComponentsForDoubleQuotes
        return (value : values)
        <|> return []
      where
        literalValue = Literal <$> many' escapeItem
        isEmptyLiteral :: ValueComponent -> Bool
        isEmptyLiteral (Literal "") = True
        isEmptyLiteral _ = False
        escapeItem = Parser parseEscapeChar
        parseEscapeChar [] = []
        parseEscapeChar ('"' : _) = []
        parseEscapeChar ('$' : '(' : _) = []
        parseEscapeChar ('$' : '{' : _) = []
        parseEscapeChar ('\\' : '\n' : xs) = [('\n', xs)]
        parseEscapeChar ('\\' : '\r' : xs) = [('\r', xs)]
        parseEscapeChar ('\\' : '\t' : xs) = [('\t', xs)]
        parseEscapeChar ('\\' : '\f' : xs) = [('\f', xs)]
        parseEscapeChar ('\\' : '\b' : xs) = [('\b', xs)]
        parseEscapeChar ('\\' : '\"' : xs) = [('\"', xs)]
        parseEscapeChar ('\\' : '\'' : xs) = [('\'', xs)]
        parseEscapeChar ('\\' : '\\' : xs) = [('\\', xs)]
        parseEscapeChar ('\\' : '$' : xs) = [('$', xs)]
        parseEscapeChar ('\\' : x : xs) = [(x, xs)]
        parseEscapeChar (x : xs) = [(x, xs)]

parseAll :: Parser [ComponentizedValue]
parseAll =
  do
    _ <- many' (spaces >> zeroOrOne comment >> newLine)
    first <- variableInDoubleQuotes <|> variableInSingleQuotes <|> unquotedVariable
    rest <- parseAll
    return (first : rest)
    <|> return []
  where
    comment :: Parser String
    comment = token (char '#') >> many' notNewLine

parsedComponentizedValues :: String -> Maybe [ComponentizedValue]
parsedComponentizedValues = parsedValue parseAll

parsedDotEnv :: ComponentEvaluationMode -> FilePath -> IO [(String, String)]
parsedDotEnv mode path =
  do
    content <- readFile path
    case parsedComponentizedValues content of
      Nothing -> pure []
      Just values -> pure . map (evaluate' mode) $ values

parsedDotEnvDebug :: FilePath -> IO [(String, String)]
parsedDotEnvDebug = parsedDotEnv Debug

parsed :: FilePath -> IO [(String, String)]
parsed = parsedDotEnv Raw
