module Parser where

import AST
import Data.Char
import Control.Monad
import qualified Data.Map as M
import Control.Applicative ((<|>), many)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec hiding(many, option, (<|>))

tokenParser = T.makeTokenParser $ emptyDef
  { commentLine = "--"
  , nestedComments = False
  , identStart = letter
  , identLetter = satisfy idLetter
  , opStart = opLetter haskellStyle
  , reservedNames =
    [ "case", "of", "end", "when"
    , "if", "fun", "and", "or", "do"
    , "then", "else", "let", "in"
    , "begin", "record" ]
  , caseSensitive = True }
    where
      idLetter c = isAlphaNum c || c == '_'

identifier :: Parser String
identifier = do
  ident <- T.identifier tokenParser
  if isLower (head ident)
  then return ident
  else fail ""

constructor :: Parser String
constructor = do
  ident <- T.identifier tokenParser
  if isUpper (head ident)
  then return ident
  else fail ""

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp tokenParser

reserved :: String -> Parser ()
reserved = T.reserved tokenParser

integer :: Parser Integer
integer = T.integer tokenParser

parens :: Parser a -> Parser a
parens = T.parens tokenParser

semi :: Parser String
semi = T.semi tokenParser

comma :: Parser String
comma = T.comma tokenParser

braces :: Parser a -> Parser a
braces = T.braces tokenParser

brackets :: Parser a -> Parser a
brackets = T.brackets tokenParser

symbol :: String -> Parser String
symbol = T.symbol tokenParser

operator :: Parser String
operator = T.operator tokenParser

charLiteral :: Parser Char
charLiteral = T.charLiteral tokenParser

stringLiteral :: Parser String
stringLiteral = T.stringLiteral tokenParser

lexeme :: Parser a -> Parser a
lexeme = T.lexeme tokenParser

whitespace :: Parser ()
whitespace = T.whiteSpace tokenParser

binOpTable :: M.Map String String
binOpTable = M.fromList
  [ ("*", "int_mul")
  , ("%", "int_rem")
  , ("/", "int_div")
  , ("+", "int_add")
  , ("-", "int_sub")
  , ("&", "int_band")
  , ("|", "int_bor")
  , ("&&", "int_and")
  , ("||", "int_or")
  , ("^", "int_xor")
  , ("<", "int_lt")
  , (">", "int_gt")
  , ("<=", "int_leq")
  , (">=", "int_geq")
  , ("==", "int_eq")
  , ("!=", "int_neq") ]

unOpTable :: M.Map String String
unOpTable = M.fromList
  [ ("-", "int_neg")
  , ("!", "int_not")
  , ("~", "int_bnot") ]

expBinOp :: String -> Assoc -> Operator Char () Exp
expBinOp op assoc = flip Infix assoc $ do
  reservedOp op
  return apply2
  where
    apply2 a b = EApply (ESymbol (Fun (binOpTable M.! op) 2)) [a,b]

expUnOp :: String -> Operator Char () Exp
expUnOp op = Prefix $ do
  reservedOp op
  return apply1
  where
    apply1 a = EApply (ESymbol (Fun (unOpTable M.! op) 1)) [a]

opTable :: [[Operator Char () Exp]]
opTable =
  [ [ expUnOp "-", expUnOp "!", expUnOp "~" ]
  , [ expBinOp "*" AssocLeft, expBinOp "/" AssocLeft
    , expBinOp "%" AssocLeft, expBinOp "&" AssocLeft ]
  , [ expBinOp "^" AssocLeft ]
  , [ expBinOp "+" AssocLeft, expBinOp "-" AssocLeft
    , expBinOp "|" AssocLeft ]
  , [ expBinOp "<" AssocNone, expBinOp ">" AssocNone
    , expBinOp "<=" AssocNone, expBinOp ">=" AssocNone
    , expBinOp "!=" AssocNone, expBinOp "==" AssocNone]
  , [ expBinOp "&&" AssocRight, expBinOp "||" AssocRight ]]

expr :: Parser Exp
expr = buildExpressionParser opTable expr1

expr1 :: Parser Exp
expr1 =
  try iteExpr
  <|> try lambdaExpr
  <|> try letExpr
  <|> try extractConsExpr
  <|> try appExpr
  <|> try consExpr
  <|> try testConsExpr
  <|> try intExpr
  <|> try beginExpr
  <|> try (parens expr)

beginExpr :: Parser Exp
beginExpr = do
  reserved "begin"
  e <- expr
  reserved "end"
  return e

letExpr :: Parser Exp
letExpr = do
  reserved "let"
  ident <- identifier
  args <- sepBy identifier whitespace
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  case args of
    [] ->
      return (ELetIn ident e1 e2)
    _:_ ->
      return (ELetIn ident (ELambda args e1) e2)

iteExpr :: Parser Exp
iteExpr = do
  reserved "if"
  i <- expr
  reserved "then"
  t <- expr
  reserved "else"
  e <- expr
  return (EIte i t e)

lambdaExpr :: Parser Exp
lambdaExpr = do
  reservedOp "\\"
  idents <- sepBy identifier whitespace
  reservedOp "->"
  e <- expr
  return (ELambda idents e)

intExpr :: Parser Exp
intExpr = do
  i <- integer
  return (ELit (fromInteger i))

consExpr :: Parser Exp
consExpr = do
  x <- constructor
  opt <- optionMaybe (parens (sepBy expr comma))
  case opt of
    Just exprs -> return (EApply (ESymbol (ConstructorMk x)) exprs)
    Nothing -> return (ESymbol (ConstructorMk x))
    --Just exprs -> return (EApply (ESymbol (globalConsFn x) (length exprs)) exprs)
    --Nothing -> return (ESymbol x 0)

testConsExpr :: Parser Exp
testConsExpr = do
  reservedOp "?"
  x <- constructor
  e <- parens expr
  return (EApply (ESymbol (ConstructorTest x)) [e])

extractConsExpr :: Parser Exp
extractConsExpr = do
  e <- identifier
  i <- brackets integer
  return (EApply (ESymbol ConstructorExtract) [(EVar e), ELit (fromInteger i)])

appExpr :: Parser Exp
appExpr = do
  x <- identifier
  opt <- optionMaybe (parens (sepBy expr comma))
  case opt of
    Just exprs -> return (EApply (EVar x) exprs)
    Nothing -> return (EVar x)

functionDeclaration :: Parser (Id, DeclBody)
functionDeclaration = do
  ident <- identifier
  args <- sepBy identifier whitespace
  reservedOp "="
  e <- expr
  return (ident, FunDecl args e)

constructorDeclaration :: Parser (Id, DeclBody)
constructorDeclaration = do
  reserved "record"
  ident <- constructor
  i <- parens integer
  return (ident, ConstructorDecl (fromInteger i))

programParser :: Parser Decls
programParser = do
  list <- many (functionDeclaration <|> constructorDeclaration)
  return (M.fromList list)

parseFile :: SourceName -> IO Decls
parseFile f = parseFromFile (programParser <* eof) f >>= \ result ->
  case result of
    Left e -> error . show $ e
    Right p -> return p
