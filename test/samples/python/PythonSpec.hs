module PythonSpec (spec) where

import Control.Applicative (Alternative ((<|>)))
import Mparse.GeneralParser ((//), (|:), (|:|))
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
value' = _val
  where
    _val = optionallyParenthesized $ (exprCombine' value' value' <|> _primitive)
    _list = ListLiteral <$> GP.bracketed (value' // (GP.token (GP.char ',')))
    _string = StringLiteral <$> GP.between (GP.char '"') (GP.char '"') (GP.repeated (GP.notChar '"'))
    _integer = IntegerLiteral <$> GP.nat
    _var = Variable <$> name'
    _primitive = _list <|> _string <|> _integer <|> _var
    _ops :: PyParser Value -> PyParser Value -> PyParser Value
    _ops ep ep' =
      ep `_plus` ep'
        <|> ep `_sub` ep'
        <|> ep `_mult` ep'
        <|> ep `_div` ep'
      where
        _op :: Op -> PyParser a -> PyParser Value -> PyParser Value -> PyParser Value
        _op op' ep'' v v' = uncurry (Expr op') <$> GP.pairOn ep'' (GP.token v) v'
        _plus = Plus `_op` _PLUS
        _sub = Sub `_op` _SUB
        _mult = Mult `_op` _MULT
        _div = Div `_op` _DIV
    opChar :: PyParser Op
    opChar =
      (const Plus <$> _PLUS)
        <|> (const Sub <$> _SUB)
        <|> (const Mult <$> _MULT)
        <|> (const Div <$> _DIV)
    exprCombine' :: PyParser Value -> PyParser Value -> PyParser Value
    exprCombine' ep ep' = do
      v <- GP.token ep
      op <- opChar
      v' <- GP.token ep'
      pure $ Expr op v v'
    optionallyParenthesized :: PyParser a -> PyParser a
    optionallyParenthesized ep = (GP.parenthesized ep) <|> ep

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
blankLine = const BlankLine <$> GP.newLine

functionApplication :: PyParser Grammar
functionApplication =
  FunctionApplication
    <$> name'
    <*> params'
  where
    params' :: PyParser [Value]
    params' = GP.parenthesized (value' // (GP.char ',' <* GP.spaces))

_PLUS :: PyParser Op
_PLUS = const Plus <$> (GP.token . GP.char $ '+')

_SUB :: PyParser Op
_SUB = const Sub <$> (GP.token . GP.char $ '-')

_MULT :: PyParser Op
_MULT = const Mult <$> (GP.token . GP.char $ '*')

_DIV :: PyParser Op
_DIV = const Div <$> (GP.token . GP.char $ '/')

_ASSIGN :: PyParser Op
_ASSIGN = const Assign <$> (GP.token $ GP.char '=')

_DEF :: PyParser String
_DEF = GP.token . GP.exact $ "def"

_COLON :: PyParser Char
_COLON = GP.token . GP.char $ ':'

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
