module PythonSpec (spec) where

import Control.Applicative (Alternative ((<|>)))
import Mparse.EParser ((//), (|:|))
import qualified Mparse.EParser as EP
import Test.Hspec

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

name' :: EP.EParser String
name' = EP.repeated1 EP.alpha

value' :: EP.EParser Value
value' = optionallyParenthesized $ (exprCombine' value' value' <|> _primitive)
  where
    _list = ListLiteral <$> EP.bracketed (value' // (EP.token (EP.char ',')))
    _string = StringLiteral <$> EP.between (EP.char '"') (EP.char '"') (EP.repeated (EP.notChar '"'))
    _integer = IntegerLiteral <$> EP.nat
    _var = Variable <$> name'
    _primitive = _list <|> _string <|> _integer <|> _var
    _ops :: EP.EParser Value -> EP.EParser Value -> EP.EParser Value
    _ops ep ep' =
      ep `_plus` ep'
        <|> ep `_sub` ep'
        <|> ep `_mult` ep'
        <|> ep `_div` ep'
      where
        _op :: Op -> EP.EParser a -> EP.EParser Value -> EP.EParser Value -> EP.EParser Value
        _op op' ep'' v v' = uncurry (Expr op') <$> EP.pairOn ep'' (EP.token v) v'
        _plus = Plus `_op` _PLUS
        _sub = Sub `_op` _SUB
        _mult = Mult `_op` _MULT
        _div = Div `_op` _DIV
    opChar :: EP.EParser Op
    opChar =
      (const Plus <$> _PLUS)
        <|> (const Sub <$> _SUB)
        <|> (const Mult <$> _MULT)
        <|> (const Div <$> _DIV)
    exprCombine' :: EP.EParser Value -> EP.EParser Value -> EP.EParser Value
    exprCombine' ep ep' = do
      v <- EP.token ep
      op <- opChar
      v' <- EP.token ep'
      pure $ Expr op v v'
    optionallyParenthesized :: EP.EParser a -> EP.EParser a
    optionallyParenthesized ep = (EP.parenthesized ep) <|> ep

assignment' :: EP.EParser Grammar
assignment' = uncurry Assignment <$> name' `equals'` value'
  where
    equals' = EP.pairOn _ASSIGN . EP.token

import' :: EP.EParser Grammar
import' =
  aliasedAbsoluteImport
    <|> absoluteImport
    <|> aliasedRelativeImport
    <|> relativeImport
  where
    importModule' :: EP.EParser String
    importModule' = _IMPORT >> name'
    fromModule' :: EP.EParser String
    fromModule' = _FROM >> name'
    alias' :: EP.EParser String
    alias' = _AS >> name'
    relativeImportModule' :: EP.EParser (String, String)
    relativeImportModule' = (EP.token fromModule') |:| importModule'
    absoluteImport = AbsoluteImport <$> importModule'
    aliasedAbsoluteImport = AliasedAbsoluteImport <$> (EP.token importModule') <*> alias'
    relativeImport = RelativeImport <$> (EP.token relativeImportModule')
    aliasedRelativeImport = AliasedRelativeImport <$> (EP.token relativeImportModule') <*> alias'

indent :: Int -> EP.EParser String
indent sc = EP.counted (4 * sc) EP.space

blankLine :: EP.EParser Grammar
blankLine = const BlankLine <$> EP.newLine

functionApplication :: EP.EParser Grammar
functionApplication =
  FunctionApplication
    <$> name'
    <*> params'
  where
    params' :: EP.EParser [Value]
    params' = EP.parenthesized (value' // (EP.char ',' <* EP.spaces))

_PLUS :: EP.EParser Op
_PLUS = const Plus <$> (EP.token . EP.char $ '+')

_SUB :: EP.EParser Op
_SUB = const Sub <$> (EP.token . EP.char $ '-')

_MULT :: EP.EParser Op
_MULT = const Mult <$> (EP.token . EP.char $ '*')

_DIV :: EP.EParser Op
_DIV = const Div <$> (EP.token . EP.char $ '/')

_ASSIGN :: EP.EParser Op
_ASSIGN = const Assign <$> (EP.token $ EP.char '=')

_DEF :: EP.EParser String
_DEF = EP.token . EP.exact $ "def"

_COLON :: EP.EParser Char
_COLON = EP.token . EP.char $ ':'

_AS :: EP.EParser String
_AS = EP.token . EP.exact $ "as"

_IMPORT :: EP.EParser String
_IMPORT = EP.token . EP.exact $ "import"

_FROM :: EP.EParser String
_FROM = EP.token . EP.exact $ "from"

_FOR :: EP.EParser String
_FOR = EP.token . EP.exact $ "for"

_WHILE :: EP.EParser String
_WHILE = EP.token . EP.exact $ "while"

_IN :: EP.EParser String
_IN = EP.token . EP.exact $ "in"

functionDefinition :: Int -> EP.EParser Grammar
functionDefinition n = definition' <*> scope (n + 1)
  where
    arguments' :: EP.EParser [Argument]
    arguments' = EP.parenthesized (name' // (EP.char ',' <* EP.spaces))
    signature' :: EP.EParser (Scope -> Grammar)
    signature' = FunctionDefinition <$> name' <*> arguments'
    definition' :: EP.EParser (Scope -> Grammar)
    definition' = EP.between _DEF (_COLON <* EP.newLine) signature'

scope :: Int -> EP.EParser Scope
scope n = Scope <$> EP.repeated1 (indent n >> inner')
  where
    line' ep = EP.token ep <* EP.newLine
    inner' :: EP.EParser Grammar
    inner' =
      (line' import')
        <|> (line' assignment')
        <|> (line' functionApplication)
        <|> functionDefinition n
        <|> blankLine

parsepy :: String -> EP.ParsedData Scope
parsepy = EP.parsed (scope 0)

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

-- let parsed' = parsepy value
-- print parsed'
-- shouldBe parsed' (EP.ParsedData expected')
