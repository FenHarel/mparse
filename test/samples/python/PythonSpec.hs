module PythonSpec (spec) where

import Control.Applicative (Alternative ((<|>)))
import Mparse.MParser ((//), (<<|), (|:), (|:|))
import qualified Mparse.MParser as MP
import Test.Hspec

type Location = (Int, Int, Int)

data ParseLocation = ParseLocation Location deriving (Show, Eq)

instance MP.ParseState ParseLocation where
  initialState = ParseLocation (0, 0, 0)
  consumeCharacter '\n' (ParseLocation (lcp, _, acp)) = ParseLocation (lcp + 1, 0, acp + 1)
  consumeCharacter _ (ParseLocation (lcp, rcp, acp)) = ParseLocation (lcp, rcp + 1, acp + 1)

type PyParser = MP.MParser ParseLocation

pyparse :: PyParser a -> String -> MP.ParsedData a [String]
pyparse = MP.parsed

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
    validName = MP.letter |: MP.repeated MP.alpha

value' :: PyParser Value
value' = do
  term' <- term
  expr' term' <|> pure term'
  where
    term :: PyParser Value
    term = parenExpr' <|> values'
      where
        parenExpr' :: PyParser Value
        parenExpr' = MP.token $ MP.parenthesized value'
        values' :: PyParser Value
        values' = MP.spaces >> MP.token valueTypes'
          where
            stringLiteral' = StringLiteral <$> MP.dQuoted (MP.repeated (MP.notChar '"'))
            integerLiteral = IntegerLiteral <$> MP.nat
            variable' = Variable <$> name'
            primitives' :: PyParser Value
            primitives' = stringLiteral' <|> integerLiteral <|> variable'
            listLiteral' = ListLiteral <$> MP.bracketed elements'
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
                params' = MP.parenthesized (value' // _COMMA)
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
    equals' = MP.pairOn _ASSIGN . MP.token

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
    relativeImportModule' = (MP.token fromModule') |:| importModule'
    absoluteImport = AbsoluteImport <$> importModule'
    aliasedAbsoluteImport = AliasedAbsoluteImport <$> (MP.token importModule') <*> alias'
    relativeImport = RelativeImport <$> (MP.token relativeImportModule')
    aliasedRelativeImport = AliasedRelativeImport <$> (MP.token relativeImportModule') <*> alias'

indent :: Int -> PyParser String
indent sc = MP.counted (4 * sc) MP.space

blankLine :: PyParser Grammar
blankLine = BlankLine <<| MP.newLine

functionApplication :: PyParser Grammar
functionApplication =
  FunctionApplication
    <$> name'
    <*> params'
  where
    params' :: PyParser [Value]
    params' = MP.parenthesized (value' // (MP.char ',' <* MP.spaces))

_PLUS :: PyParser Op
_PLUS = Plus <<| (MP.token . MP.char $ '+')

_SUB :: PyParser Op
_SUB = Sub <<| (MP.token . MP.char $ '-')

_MULT :: PyParser Op
_MULT = Mult <<| (MP.token . MP.char $ '*')

_DIV :: PyParser Op
_DIV = Div <<| (MP.token . MP.char $ '/')

_ASSIGN :: PyParser Op
_ASSIGN = Assign <<| (MP.token $ MP.char '=')

_DEF :: PyParser String
_DEF = MP.token . MP.exact $ "def"

_COLON :: PyParser Char
_COLON = MP.token . MP.char $ ':'

_COMMA :: PyParser Char
_COMMA = MP.token . MP.char $ ','

_AS :: PyParser String
_AS = MP.token . MP.exact $ "as"

_IMPORT :: PyParser String
_IMPORT = MP.token . MP.exact $ "import"

_FROM :: PyParser String
_FROM = MP.token . MP.exact $ "from"

_FOR :: PyParser String
_FOR = MP.token . MP.exact $ "for"

_WHILE :: PyParser String
_WHILE = MP.token . MP.exact $ "while"

_IN :: PyParser String
_IN = MP.token . MP.exact $ "in"

functionDefinition :: Int -> PyParser Grammar
functionDefinition n = definition' <*> scope (n + 1)
  where
    arguments' :: PyParser [Argument]
    arguments' = MP.parenthesized (name' // (MP.char ',' <* MP.spaces))
    signature' :: PyParser (Scope -> Grammar)
    signature' = FunctionDefinition <$> name' <*> arguments'
    definition' :: PyParser (Scope -> Grammar)
    definition' = MP.between _DEF (_COLON <* MP.newLine) signature'

scope :: Int -> PyParser Scope
scope n = Scope <$> MP.repeated1 (indent n >> inner')
  where
    line' ep = MP.token ep <* MP.newLine
    inner' :: PyParser Grammar
    inner' =
      (line' import')
        <|> (line' assignment')
        <|> (line' functionApplication)
        <|> functionDefinition n
        <|> blankLine

parsepy :: String -> MP.ParsedData Scope [String]
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
    let parsed' = parsepy value
    shouldBe parsed' (MP.ParsedData expected')
