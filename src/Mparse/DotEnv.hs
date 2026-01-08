module Mparse.DotEnv where

import Control.Applicative (Alternative ((<|>)))
import Mparse.GeneralParser

type Location = (Int, Int, Int)

data ParseLocation = ParseLocation Location deriving (Show, Eq)

instance ParseState ParseLocation where
  initialState = ParseLocation (0, 0, 0)
  consumeCharacter '\n' (ParseLocation (lcp, _, acp)) = ParseLocation (lcp + 1, 0, acp + 1)
  consumeCharacter _ (ParseLocation (lcp, rcp, acp)) = ParseLocation (lcp, rcp + 1, acp + 1)

type DotEnvParser = GeneralParser ParseLocation

type KeyValue = (String, String)

dotenvparse :: DotEnvParser a -> String -> GeneralParsedData a [String]
dotenvparse = parsed

data ValueComponent
  = Literal String
  | Variable String
  | Command String
  deriving (Show, Eq)

escape :: Char -> DotEnvParser Char
escape c = char '\\' >> char c

variable :: DotEnvParser ValueComponent
variable = between openSub closeSub var
  where
    openSub = sat (== '$') |:| sat (== '{')
    closeSub = sat (== '}')
    var :: DotEnvParser ValueComponent
    var = Variable <$> repeated1 validCharacter
      where
        escapedClose :: DotEnvParser Char
        escapedClose = escape '}'
        notClose :: DotEnvParser Char
        notClose = notChar '}'
        validCharacter :: DotEnvParser Char
        validCharacter = (escapedClose <|> notClose)

command :: DotEnvParser ValueComponent
command = between openSub closeSub com
  where
    openSub = sat (== '$') |:| sat (== '(')
    closeSub = sat (== ')')
    com :: DotEnvParser ValueComponent
    com = Command <$> repeated1 validCharacter
      where
        escapedClose :: DotEnvParser Char
        escapedClose = escape ')'
        notClose :: DotEnvParser Char
        notClose = notChar ')'
        validCharacter :: DotEnvParser Char
        validCharacter = (escapedClose <|> notClose)

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

valueComponentsForDoubleQuotes' :: DotEnvParser [ValueComponent]
valueComponentsForDoubleQuotes' =
  do
    value <- variable <|> command <|> literalValue
    values <- if isEmptyLiteral value then return [] else valueComponentsForDoubleQuotes'
    return (value : values)
    <|> return []
  where
    literalValue = Literal <$> repeated validCharacter
    isEmptyLiteral :: ValueComponent -> Bool
    isEmptyLiteral (Literal "") = True
    isEmptyLiteral _ = False
    escapedCharacters :: DotEnvParser Char
    escapedCharacters =
      escape '\n'
        <|> escape '\r'
        <|> escape '\t'
        <|> escape '\f'
        <|> escape '\b'
        <|> escape '\"'
        <|> escape '\''
        <|> escape '\\'
        <|> escape '$'
        <|> (char '\\' >> item)
        <|> item
    validCharacter :: DotEnvParser Char
    validCharacter = blacklist' |-| escapedCharacters
      where
        blacklist' :: DotEnvParser Blacklist
        blacklist' =
          blacklisted (char '"')
            <|> blacklisted (exact "$(")
            <|> blacklisted (exact "${")

equals' :: DotEnvParser a -> DotEnvParser b -> DotEnvParser (a, b)
equals' = pairOn (token $ char '=')

identifier :: DotEnvParser String
identifier = alpha_ |: repeated (alpha_ <|> digit)
  where
    alpha_ = letter <|> char '_'

unquotedVariable :: DotEnvParser ComponentizedValue
unquotedVariable =
  do
    (name, components) <- identifier `equals'` valueComponents
    return (ComponentizedValue name components)
  where
    valueComponents :: DotEnvParser [ValueComponent]
    valueComponents =
      do
        value <- command <|> variable <|> literalValue
        values <- valueComponents
        return (value : values)
        <|> return []
      where
        literalValue = Literal <$> repeated1 validCharacters
        validCharacters :: DotEnvParser Char
        validCharacters = blacklist' |-| escapedCharacters
          where
            escapedCharacters :: DotEnvParser Char
            escapedCharacters = (char '\\' >> char '$') <|> item
            blacklist' :: DotEnvParser Blacklist
            blacklist' =
              blacklisted (char ' ')
                <|> blacklisted (char '\t')
                <|> blacklisted (char '#')
                <|> blacklisted (char '\n')
                <|> blacklisted (exact "$(")
                <|> blacklisted (exact "${")

variableInSingleQuotes :: DotEnvParser ComponentizedValue
variableInSingleQuotes =
  do
    (name, value) <- identifier `equals'` valueInSingleQuotes
    return (ComponentizedValue name [Literal value])
  where
    notSq = sat (/= '\'')
    valueInSingleQuotes = token $ sQuoted (repeated notSq)

variableInDoubleQuotes :: DotEnvParser ComponentizedValue
variableInDoubleQuotes =
  do
    (name, results) <- identifier `equals'` valueComponents
    return (ComponentizedValue name results)
  where
    valueComponents :: DotEnvParser [ValueComponent]
    valueComponents = token $ dQuoted valueComponentsForDoubleQuotes
    valueComponentsForDoubleQuotes :: DotEnvParser [ValueComponent]
    valueComponentsForDoubleQuotes =
      do
        value <- variable <|> command <|> literalValue
        values <- if isEmptyLiteral value then return [] else valueComponentsForDoubleQuotes
        return (value : values)
        <|> return []
      where
        literalValue = Literal <$> repeated validCharacters
        isEmptyLiteral :: ValueComponent -> Bool
        isEmptyLiteral (Literal "") = True
        isEmptyLiteral _ = False
        validCharacters :: DotEnvParser Char
        validCharacters = blacklist' |-| escapedCharacters
          where
            blacklist' :: DotEnvParser Blacklist
            blacklist' =
              blacklisted (char '"')
                <|> blacklisted (exact "$(")
                <|> blacklisted (exact "${")
            escapedCharacters :: DotEnvParser Char
            escapedCharacters =
              escape '\n'
                <|> escape '\r'
                <|> escape '\t'
                <|> escape '\f'
                <|> escape '\b'
                <|> escape '\"'
                <|> escape '\''
                <|> escape '\\'
                <|> escape '$'
                <|> (char '\\' >> item)
                <|> item

parseAll :: DotEnvParser [ComponentizedValue]
parseAll =
  do
    _ <- repeated (spaces >> optional comment >> newLine)
    first <- variableInDoubleQuotes <|> variableInSingleQuotes <|> unquotedVariable
    rest <- parseAll
    return (first : rest)
    <|> return []
  where
    comment :: DotEnvParser String
    comment = token (char '#') >> repeated notNewLine

parsedComponentizedValues :: String -> Maybe [ComponentizedValue]
parsedComponentizedValues input = case dotenvparse parseAll input of
  ParsedData a -> Just a
  _ -> Nothing

parsedDotEnv :: ComponentEvaluationMode -> FilePath -> IO [(String, String)]
parsedDotEnv mode path =
  do
    content <- readFile path
    case parsedComponentizedValues content of
      Nothing -> pure []
      Just values -> pure . map (evaluate' mode) $ values

parsedDotEnvDebug :: FilePath -> IO [(String, String)]
parsedDotEnvDebug = parsedDotEnv Debug

parsedFile :: FilePath -> IO [(String, String)]
parsedFile = parsedDotEnv Raw
