module Parse where

import Lexer
import qualified AST

import Data.Char
import Data.List

import Text.Parsec hiding(token,State)
import qualified Text.Parsec

import Control.Monad.Identity
import Text.Parsec.Pos(newPos)

import qualified Data.Map as M
import qualified Data.Set as S

-- | Fixity of infix operators
data Fixity
  = FixNone
  | FixLeft
  | FixRight
  deriving(Eq, Show)

-- | Map from operators to their associated fixities
type Fixities = M.Map String (Int, Fixity)

-- | Find the fixities of the operators in a program
findFixities :: [Token] -> Fixities
findFixities (TIdent _ "infixl" : TInt _ i : TOper _ op : xs) =
  M.insert op (i, FixLeft) (findFixities xs)
findFixities (TIdent _ "infixr" : TInt _ i : TOper _ op : xs) =
  M.insert op (i, FixRight) (findFixities xs)
findFixities (TIdent _ "infix" : TInt _ i : TOper _ op : xs) =
  M.insert op (i, FixNone) (findFixities xs)
findFixities (TIdent _ "infixl" : TInt _ i : TSpec _ '`' : TIdent _ op : TSpec _ '`' : xs) =
  M.insert op (i, FixLeft) (findFixities xs)
findFixities (TIdent _ "infixr" : TInt _ i : TSpec _ '`' : TIdent _ op : TSpec _ '`' : xs) =
  M.insert op (i, FixRight) (findFixities xs)
findFixities (TIdent _ "infix" : TInt _ i : TSpec _ '`' : TIdent _ op : TSpec _ '`' : xs) =
  M.insert op (i, FixNone) (findFixities xs)
findFixities (_:xs) = findFixities xs
findFixities [] = M.empty

data State =
  State
    { fixities :: Fixities }

-- A parser must be able to read the fixities of the operators so it use an object of type
-- @Fixities@ as a state, and a liist of token as an input stream
type Parser = ParsecT [Token] State Identity

-- | Utility to parse a token
token :: (Token -> Maybe a) -> Parser a
token f =
  Text.Parsec.token show (\ t -> let s = tokenSLoc t in newPos "" (line s) (column s)) f

-- | List of reserved keywords in haskell 2010
keywords :: [String]
keywords =
  ["_primitive", "case", "class", "data", "default", "deriving", "do", "else", "forall", "foreign"
  , "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of"
  , "pattern", "then", "type", "where"]

-- | Parse an identifier, it can't be a keyword
identifier :: Parser String
identifier = token go
  where
    go (TIdent _ s) = if elem s keywords then Nothing else Just s
    go _ = Nothing

-- | Parse an identifier starting by a lower case, it can't be a keyword
variable :: Parser String
variable = token go
  where
    go (TIdent _ (x:xs)) = if elem (x:xs) keywords || isUpper x then Nothing else Just (x:xs)
    go _ = Nothing

-- | Parse an identifier starting by a upper case
constructor :: Parser String
constructor = token go
  where
    go (TIdent _ (x:xs)) = if elem (x:xs) keywords || isLower x then Nothing else Just (x:xs)
    go _ = Nothing

-- | Parse a given keyword
keyword :: String -> Parser ()
keyword k = token go
  where
    go (TIdent _ s) = if s == k then Just () else Nothing
    go _ = Nothing

-- | Parse an integer
integer :: Parser Int
integer = token go
  where
    go (TInt _ s) = Just s
    go _ = Nothing

-- | Parse a specifier
spec :: Char -> Parser ()
spec c = token go
  where
    go (TSpec _ c') = if c == c' then Just () else Nothing
    go _ = Nothing

-- | Parse an operator
operator :: Parser String
operator = token go1 <|> go2
  where
    go1 (TOper _ o) | not (elem o ["->","<-","=>","::"]) = Just o
    go1 _ = Nothing
    go2 = do
      spec '`'
      i <- identifier
      spec '`'
      return i

-- | Parse a given operator
reservedOp :: String -> Parser ()
reservedOp s = token go
  where
    go (TOper _ s') = if s' == s then Just () else Nothing
    go _ = Nothing

-- | Parse operations in parenthesis
parens :: Parser a -> Parser a
parens p = do
  spec '('
  x <- p
  spec ')'
  return x

-- | Parse operations in braces
braces :: Parser a -> Parser a
braces p = do
  spec '{'
  x <- p
  spec '}'
  return x

-- | Parse operations in brackets
brackets :: Parser a -> Parser a
brackets p = do
  spec '['
  x <- p
  spec ']'
  return x

typ :: Parser AST.Type
typ = do
  forallType
  <|> ftype

ftype :: Parser AST.Type
ftype = do
  arg <- btype
  option arg $ do
    reservedOp "->"
    res <- typ
    return (AST.Arrow arg res)

btype :: Parser AST.Type
btype = do
  atype
  -- atypes <- many1 atype
  -- return (go atypes)
  --   where
  --     go [t] = t
  --     go (t:ts) = AST.Arrow t (go ts)
  --     go [] = error "a btype must have at least one atype"

atype :: Parser AST.Type
atype = do
  varType
  <|> parens typ

forallType :: Parser AST.Type
forallType = do
  keyword "forall"
  idents <- many1 identifier
  spec '.'
  ty <- typ
  return (AST.Forall (AST.BoundTv <$> idents) ty)

varType :: Parser AST.Type
varType = do
  ident <- identifier
  if ident == "int"
  then return AST.TInt
  else return (AST.TVar $ AST.BoundTv ident)

expr :: Parser AST.Exp
expr = do
  ret <- infixexp
  option ret $ do
    reservedOp "::"
    ty <- typ
    return (AST.Annot ret ty)

infixexp :: Parser AST.Exp
infixexp = do
  list <- many go
  state <- getState
  case resolveFixities (fixities state) list of
    Just e -> pure e
    Nothing -> fail ""
  where
    go = (Left <$> lexp) <|> Right <$> operator

lexp :: Parser AST.Exp
lexp = do
  lambdaExp
  <|> caseExp
  <|> ifExp
  <|> fexp

-- | A function application is simply a list of aexpr (aexpr are identifiers, expressions with
-- parenthesis...)
fexp :: Parser AST.Exp
fexp = do
  list <- many1 aexp
  case list of
    hd : [] -> return hd
    hd : tl -> return (AST.Apply hd tl)
    _ -> error ""

aexp :: Parser AST.Exp
aexp =
  identExp
  <|> letExp
  <|> litExp
  <|> parens expr

identExp :: Parser AST.Exp
identExp = AST.Var <$> identifier

litExp :: Parser AST.Exp
litExp = AST.Lit . AST.Int <$> integer

lambdaExp :: Parser AST.Exp
lambdaExp = do
  spec '\\'
  args <- many1 identifier
  reservedOp "->"
  body <- expr
  return (AST.Lambda args body)

pattern :: Parser AST.Pattern
pattern = p0
  where
    p0 =
      consPat
      <|> p1
    p1 =
      wildcard
      <|> varPat
      <|> parens p0
    wildcard = reservedOp "_" >> pure AST.Wildcard
    varPat = AST.PVar <$> identifier
    consPat = do
      c <- constructor
      pats <- many p1
      return (AST.PCons c pats)

caseExp :: Parser AST.Exp
caseExp = do
  keyword "case"
  e <- expr
  keyword "of"
  list <- braces (sepBy (pattern_and_expr) (spec ';'))
  return (AST.Case e list)
  where
    pattern_and_expr = do
      p <- pattern
      reservedOp "->"
      e <- expr
      return (p,e)

letExp :: Parser AST.Exp
letExp = do
  -- TODO: add multiple definitions in let-in expressions
  keyword "let"
  spec '{'
  fn <- identifier <|> parens operator
  let args = [] --args <- many identifier
  spec '='
  e1 <- expr
  spec '}'
  keyword "in"
  e2 <- expr
  if length args == 0
  then return (AST.LetIn fn e1 e2)
  else return (AST.LetIn fn (AST.Lambda args e1) e2)

ifExp :: Parser AST.Exp
ifExp = do
  keyword "if"
  i <- expr
  option () (spec ';')
  keyword "then"
  t <- expr
  option () (spec ';')
  keyword "else"
  e <- expr
  return (AST.Switch i [(AST.Int 0,e), (AST.Undefined, t)])


-- | See https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3
resolveFixities :: Fixities -> [Either AST.Exp String] -> Maybe AST.Exp
resolveFixities fixities toks = fmap fst $ parseUnOp (-1) FixNone toks
  where
    parseUnOp p f (Left e : rest) = parseBinOp p f e rest
    parseUnOp p1 f1 (Right "-" : rest) = do
      (r, rest') <- parseUnOp 6 FixLeft rest
      parseBinOp p1 f1 (AST.Apply (AST.Var "-") [AST.Lit (AST.Int 0), r]) rest'
    parseUnOp _ _ _ = Nothing

    parseBinOp _ _ e [] = Just (e, [])
    parseBinOp p1 f1 e1 (Right op2 : rest)
      | p1 == prec op2 && (f1 /= fix op2 || f1 == FixNone) =
        Nothing
      | p1 > prec op2 || (p1 == prec op2 && f1 == FixLeft) =
        Just (e1, Right op2 : rest)
      | otherwise = do
        (r,rest') <- parseUnOp (prec op2) (fix op2) rest
        parseBinOp p1 f1 (AST.Apply (AST.Var op2) [e1,r]) rest'
    parseBinOp _ _ _ _ = Nothing

    fix op =
      case M.lookup op fixities of
        Just (_,f) -> f
        _ -> FixNone

    prec op =
      case M.lookup op fixities of
        Just (p,_) -> p
        _ -> 1

parseWhere :: Parser [(String, [String], AST.Exp)]
parseWhere = do
  keyword "where"
  braces $ do
    sepBy functionDeclaration (spec ';')

functionAnnotation :: Parser (String, AST.Type)
functionAnnotation = do
  ident <- identifier <|> parens operator
  reservedOp "::"
  ty <- typ
  return (ident,ty)

functionDeclaration :: Parser (String, [String], AST.Exp)
functionDeclaration = do
  ident <- identifier <|> parens operator
  args <- many identifier
  spec '='
  body <- expr
  w <- option [] parseWhere
  return (ident, args, addLetIn w body)
    where
      -- TODO: add mutual recursion in where
      addLetIn ((name,args,body):decls) e =
        AST.LetIn name (AST.Lambda args body) (addLetIn decls e)
      addLetIn [] e = e

foreignDeclaration :: Parser (String, AST.Exp)
foreignDeclaration = do
  keyword "foreign"
  name <- identifier <|> parens operator
  reservedOp "::"
  ty <- typ
  let args = take (arity ty) (["a"++show i | i <- [0..]] Data.List.\\ [name])
  let lit = AST.Lit (AST.CFun name (arity ty))
  return $
    (name, AST.Annot
     (AST.Lambda args (AST.Apply lit (AST.Var <$> args)))
     ty )
  where
    arity (AST.Arrow _ t) = 1+arity t
    arity (AST.Forall _ t) = arity t
    arity _ = 0

importParser :: Parser [String]
importParser = many (keyword "import" >> identifier)

globalDecl :: Parser [(String, AST.DeclBody)]
globalDecl =
  parse_infixl
  <|> parse_infixr
  <|> parse_infix
  <|> parse_fundecl
  <|> parse_foreign
    where
      parse_infixl = keyword "infixl" >> integer >> operator >> pure []
      parse_infixr = keyword "infixr" >> integer >> operator >> pure []
      parse_infix = keyword "infix" >> integer >> operator >> pure []
      parse_fundecl = do
        (name,args,body) <- functionDeclaration
        pure [(name,AST.FunDecl args body)]
      parse_foreign = do
        (name,body) <- foreignDeclaration
        pure [(name,AST.FunDecl [] body)]

program :: Parser AST.Decls
program = do
  keyword "module"
  _ <- constructor
  keyword "where"
  decls <- braces (sepBy globalDecl (spec ';'))
  return $ M.fromList $ concat decls
