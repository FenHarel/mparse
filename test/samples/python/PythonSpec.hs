module PythonSpec (spec) where

import Control.Applicative (Alternative ((<|>)))
import Mparse.GeneralParser ((//), (<<|), (|:), (|:|))
import qualified Mparse.GeneralParser as GP
import Test.Hspec

type Location = (Int, Int, Int)

data ParseLocation = ParseLocation Location deriving (Show, Eq)

instance GP.ParseState ParseLocation where
  initialState = ParseLocation (0, 0, 0)
  consumeCharacter '\n' (ParseLocation (lcp, _, acp)) = ParseLocation (lcp + 1, 0, acp + 1)
  consumeCharacter _ (ParseLocation (lcp, rcp, acp)) = ParseLocation (lcp, rcp + 1, acp + 1)

type PyParser = GP.GeneralParser ParseLocation

pyparse :: PyParser a -> String -> GP.GeneralParsedData a [String]
pyparse = GP.parsed

type Alias = String

type Argument = String

type FunctionName = String

data Op
  = Plus
  | Sub
  | Mult
  | Div
  | Assign
  | Equality
  | Greater
  | GreaterEq
  | Lesser
  | LesserEq
  deriving (Show, Eq)

data Value
  = StringLiteral String
  | IntegerLiteral Int
  | Variable String
  | ListLiteral [Value]
  | FunctionEvaluation FunctionName [Value]
  | Expr Op Value Value
  deriving (Show, Eq)

data Grammar
  = RelativeImport (String, String)
  | AliasedRelativeImport (String, String) Alias
  | AbsoluteImport String
  | AliasedAbsoluteImport String Alias
  | Assignment String Value
  | FunctionDefinition FunctionName [Argument] Scope
  | FunctionApplication FunctionName [Value]
  | BlankLine
  deriving (Show, Eq)

data Scope = Scope [Grammar] deriving (Show, Eq)

name' :: PyParser String
name' = validName
  where
    validName = GP.letter |: GP.repeated GP.alpha

value' :: PyParser Value
value' = do
  term' <- term
  expr' term' <|> pure term'
  where
    term :: PyParser Value
    term = parenExpr' <|> values'
      where
        parenExpr' :: PyParser Value
        parenExpr' = GP.token $ GP.parenthesized value'
        values' :: PyParser Value
        values' = GP.spaces >> GP.token valueTypes'
          where
            stringLiteral' = StringLiteral <$> GP.dQuoted (GP.repeated (GP.notChar '"'))
            integerLiteral = IntegerLiteral <$> GP.nat
            variable' = Variable <$> name'
            primitives' :: PyParser Value
            primitives' = stringLiteral' <|> integerLiteral <|> variable'
            listLiteral' = ListLiteral <$> GP.bracketed elements'
              where
                elements' :: PyParser [Value]
                elements' = value' // _COMMA
            functionEvaluation' :: PyParser Value
            functionEvaluation' =
              FunctionEvaluation
                <$> name'
                <*> params'
              where
                params' :: PyParser [Value]
                params' = GP.parenthesized (value' // _COMMA)
            recursives' :: PyParser Value
            recursives' = functionEvaluation' <|> listLiteral'
            valueTypes' :: PyParser Value
            valueTypes' = recursives' <|> primitives'

    expr' :: Value -> PyParser Value
    expr' left' = do
      op <- operator
      right' <- term
      let expression = Expr op left' right'
      expr' expression <|> pure expression
      where
        operator :: PyParser Op
        operator =
          (Plus <<| _PLUS)
            <|> (Sub <<| _SUB)
            <|> (Mult <<| _MULT)
            <|> (Div <<| _DIV)

assignment' :: PyParser Grammar
assignment' = uncurry Assignment <$> name' `equals'` value'
  where
    equals' = GP.pairOn _ASSIGN . GP.token

import' :: PyParser Grammar
import' =
  aliasedAbsoluteImport
    <|> absoluteImport
    <|> aliasedRelativeImport
    <|> relativeImport
  where
    importModule' :: PyParser String
    importModule' = _IMPORT >> name'
    fromModule' :: PyParser String
    fromModule' = _FROM >> name'
    alias' :: PyParser String
    alias' = _AS >> name'
    relativeImportModule' :: PyParser (String, String)
    relativeImportModule' = (GP.token fromModule') |:| importModule'
    absoluteImport = AbsoluteImport <$> importModule'
    aliasedAbsoluteImport = AliasedAbsoluteImport <$> (GP.token importModule') <*> alias'
    relativeImport = RelativeImport <$> (GP.token relativeImportModule')
    aliasedRelativeImport = AliasedRelativeImport <$> (GP.token relativeImportModule') <*> alias'

indent :: Int -> PyParser String
indent sc = GP.counted (4 * sc) GP.space

blankLine :: PyParser Grammar
blankLine = BlankLine <<| GP.newLine

functionApplication :: PyParser Grammar
functionApplication =
  FunctionApplication
    <$> name'
    <*> params'
  where
    params' :: PyParser [Value]
    params' = GP.parenthesized (value' // (GP.char ',' <* GP.spaces))

_PLUS :: PyParser Op
_PLUS = Plus <<| (GP.token . GP.char $ '+')

_SUB :: PyParser Op
_SUB = Sub <<| (GP.token . GP.char $ '-')

_MULT :: PyParser Op
_MULT = Mult <<| (GP.token . GP.char $ '*')

_DIV :: PyParser Op
_DIV = Div <<| (GP.token . GP.char $ '/')

_ASSIGN :: PyParser Op
_ASSIGN = Assign <<| (GP.token $ GP.char '=')

_DEF :: PyParser String
_DEF = GP.token . GP.exact $ "def"

_COLON :: PyParser Char
_COLON = GP.token . GP.char $ ':'

_COMMA :: PyParser Char
_COMMA = GP.token . GP.char $ ','

_AS :: PyParser String
_AS = GP.token . GP.exact $ "as"

_IMPORT :: PyParser String
_IMPORT = GP.token . GP.exact $ "import"

_FROM :: PyParser String
_FROM = GP.token . GP.exact $ "from"

_FOR :: PyParser String
_FOR = GP.token . GP.exact $ "for"

_WHILE :: PyParser String
_WHILE = GP.token . GP.exact $ "while"

_IN :: PyParser String
_IN = GP.token . GP.exact $ "in"

functionDefinition :: Int -> PyParser Grammar
functionDefinition n = definition' <*> scope (n + 1)
  where
    arguments' :: PyParser [Argument]
    arguments' = GP.parenthesized (name' // (GP.char ',' <* GP.spaces))
    signature' :: PyParser (Scope -> Grammar)
    signature' = FunctionDefinition <$> name' <*> arguments'
    definition' :: PyParser (Scope -> Grammar)
    definition' = GP.between _DEF (_COLON <* GP.newLine) signature'

scope :: Int -> PyParser Scope
scope n = Scope <$> GP.repeated1 (indent n >> inner')
  where
    line' ep = GP.token ep <* GP.newLine
    inner' :: PyParser Grammar
    inner' =
      (line' import')
        <|> (line' assignment')
        <|> (line' functionApplication)
        <|> functionDefinition n
        <|> blankLine

parsepy :: String -> GP.GeneralParsedData Scope [String]
parsepy = pyparse (scope 0)

testFilePath :: String
testFilePath = "test/samples/python/test.py"

expected' :: Scope
expected' =
  ( Scope
      [ AbsoluteImport "os",
        RelativeImport ("pprint", "pprint"),
        BlankLine,
        FunctionDefinition
          "foo"
          ["a", "b"]
          ( Scope
              [ Assignment "w" (IntegerLiteral 4),
                Assignment "x" (StringLiteral "hello"),
                Assignment "y" (Variable "a"),
                Assignment "z" (ListLiteral [(IntegerLiteral 4), (StringLiteral "hello"), (Variable "a")]),
                Assignment "q" (Expr Plus (Expr Mult (Variable "a") (Variable "b")) (Variable "w")),
                Assignment "q" (Expr Plus (Expr Mult (Variable "a") (Variable "b")) (Variable "w")),
                Assignment "q" (Expr Mult (Variable "a") (Expr Plus (Variable "b") (Variable "w"))),
                Assignment "r" (Expr Sub (Expr Plus (Variable "a") (Variable "b")) (Variable "w")),
                Assignment "s" (Expr Mult (Variable "a") (Expr Div (Variable "b") (Variable "w"))),
                Assignment "t" (Expr Plus (Variable "a") (FunctionEvaluation "print" [IntegerLiteral 1, Variable "a", StringLiteral "b", ListLiteral [StringLiteral "hello"]])),
                FunctionApplication "print" [StringLiteral "hello world!"]
              ]
          )
      ]
  )

spec :: Spec
spec = do
  it "python" $ do
    value <- readFile testFilePath
    print value
    let parsed' = parsepy value
    print parsed'
    shouldBe parsed' (GP.ParsedData expected')
