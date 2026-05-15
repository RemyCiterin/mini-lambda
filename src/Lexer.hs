module Lexer where

import Data.Char

-- | Line in a source location
type Line = Int

-- | Column in a source location
type Col = Int

-- | Source location
data SLoc = SLoc Line Col deriving(Show)

-- | Return the line of a source location
line :: SLoc -> Line
line (SLoc l _) = l

-- | Return the column of a source location
column :: SLoc -> Col
column (SLoc _ c) = c

-- | Tokens
data Token
  = TIdent   SLoc String
  -- ^ Identifier
  | TOper    SLoc String
  -- ^ Operator
  | TString  SLoc String
  -- ^ String
  | TWildcard SLoc
  -- ^ @_@ in patterns
  | TChar    SLoc Char
  -- ^ Character literal
  | TInt     SLoc Int
  -- ^ Integer
  | TSpec    SLoc Char
  -- Speficier like: ()[]{},`<>;
  | TError   SLoc String
  -- Error in the source code
  | TBrace   SLoc
  -- Intermediate token in the lexing used to compute layout, correspond to @{n}@ in chapter 10 of
  -- the 2010 haskell reference, used to represent a place where a character @{@ sould be inserted
  | TIndent  SLoc
  -- Intermediate token in the lexing used to compute layout, correspond to @<n>@ in chapter 10 of
  -- the 2010 haskell reference, used to represent the begining of a line

instance Show Token where
  show (TBrace  (SLoc _ c)) = "{"++show c++"}"
  show (TIndent (SLoc _ c)) = "<"++show c++">"
  show (TIdent  _ i) = i
  show (TOper   _ o) = o
  show (TString _ s) = s
  show (TChar   _ c) = [c]
  show (TInt    _ i) = show i
  show (TSpec   _ s) = [s]
  show (TError  _ e) = "ERROR: " ++ e
  show (TWildcard _) = "_"

-- Increment the line of a source location
incrLine :: SLoc -> SLoc
incrLine (SLoc l _) = SLoc (l+1) 1

-- Increment the column of a source location
incrCol :: SLoc -> SLoc
incrCol (SLoc l c) = SLoc l (c+1)

-- Increment the column of a source location as many times at the length of a list
incrCols :: SLoc -> String -> SLoc
incrCols sloc (_:xs) = incrCols (incrCol sloc) xs
incrCols sloc [] = sloc

-- Increment the column of a source location using a tabulation
incrTab :: SLoc -> SLoc
incrTab (SLoc l c) = SLoc l (c+4)

-- Add an indentation annotation at the begining of a list of tokens
addIndent :: [Token] -> [Token]
addIndent l@(TIndent _:_) = l
addIndent (x:xs) = TIndent (tokenSLoc x) : x : xs
addIndent [] = []

-- | Lexing without layout resolution
-- TODO: add strings and lists
lexerNoLayout :: SLoc -> String -> [Token]
lexerNoLayout _ [] = []
lexerNoLayout loc (' ':xs) = lexerNoLayout (incrCol loc) xs
lexerNoLayout loc ('\t':xs) = lexerNoLayout (incrTab loc) xs
lexerNoLayout loc ('\n':xs) = addIndent (lexerNoLayout (incrLine loc) xs)
lexerNoLayout loc (x:xs) | isAlpha x || x == '_' =
  let (ident, rest) = span (\ y -> isAlphaNum y || y == '_') (x:xs) in
  if ident == "_" then TWildcard loc : lexerNoLayout (incrCols loc ident) rest
  else TIdent loc ident : lexerNoLayout (incrCols loc ident) rest
lexerNoLayout loc ('-':'-':xs) = addIndent (lexerNoLayout (incrLine loc) (snd (span (/= '\n') xs)))
lexerNoLayout loc (x:xs) | isSpec x = TSpec loc x : lexerNoLayout (incrCol loc) xs
lexerNoLayout loc (x:(y:ys)) | isSpecAndOper x && not (isOper y) =
  TSpec loc x : lexerNoLayout (incrCol loc) (y:ys)
lexerNoLayout loc (x:xs) | isOper x =
  let (oper, rest) = span isOper (x:xs) in
  TOper loc oper : lexerNoLayout (incrCols loc oper) rest
lexerNoLayout loc ('0':b:xs) | b == 'b' || b == 'B' =
  let (int, rest) = span (\c -> isAlphaNum c || c == '_') xs in
  case lexInt 2 int of
    Just i -> TInt loc i : lexerNoLayout (incrCols loc ('0':'b':int)) rest
    Nothing -> [TError loc ("lexing error at position: "++show loc)]
lexerNoLayout loc ('0':o:xs) | o == 'o' || o == 'O' =
  let (int, rest) = span (\c -> isAlphaNum c || c == '_') xs in
  case lexInt 8 int of
    Just i -> TInt loc i : lexerNoLayout (incrCols loc ('0':'o':int)) rest
    Nothing -> [TError loc ("lexing error at position: "++show loc)]
lexerNoLayout loc ('0':x:xs) | x == 'x' || x == 'X' =
  let (int, rest) = span (\c -> isAlphaNum c || c == '_') xs in
  case lexInt 16 int of
    Just i -> TInt loc i : lexerNoLayout (incrCols loc ('0':'x':int)) rest
    Nothing -> [TError loc ("lexing error at position: "++show loc)]
lexerNoLayout loc (x:xs) | isDigit x =
  let (int, rest) = span (\c -> isAlphaNum c || c == '_') (x:xs) in
  case lexInt 10 int of
    Just i -> TInt loc i : lexerNoLayout (incrCols loc int) rest
    Nothing -> [TError loc ("lexing error at position: "++show loc)]
lexerNoLayout loc _ = [TError loc ("lexing: unexpected character at location: "++show loc)]

-- | Parse an integer in a arbitrary base
lexInt :: Int -> String -> Maybe Int
lexInt base i = fst (go i)
  where
    go [] = (Just 0,1)
    go ('_':xs) = go xs
    go (x:xs) | isDigit x =
      case go xs of
        (Just n, m) ->
          if (digitToInt x) < base then (Just (n + m * digitToInt x), m*base)
          else (Nothing, 0)
        _ -> (Nothing, 0)
    go _ = (Nothing, 0)

-- | Return if a character is used in specifiers but not in operations
isSpec :: Char -> Bool
isSpec c =
  elem c "()[]{},;`"

-- | Return if a character is used in operands and specifiers
isSpecAndOper :: Char -> Bool
isSpecAndOper c =
  elem c "=|\\@!~"

-- | Return if a character correspond to an operation
isOper :: Char -> Bool
isOper c =
  elem c "@\\=+-!:<>.^#$*%/|&~?"

-- | Return the begining location of a token
tokenSLoc :: Token -> SLoc
tokenSLoc (TIdent    l _) = l
tokenSLoc (TOper     l _) = l
tokenSLoc (TString   l _) = l
tokenSLoc (TChar     l _) = l
tokenSLoc (TInt      l _) = l
tokenSLoc (TSpec     l _) = l
tokenSLoc (TError    l _) = l
tokenSLoc (TWildcard l)  = l
tokenSLoc (TBrace    l)   = l
tokenSLoc (TIndent   l)   = l

-- | Return if an identifier need to be followed by a brace
needBrace :: String -> Bool
needBrace s = elem s ["where", "do", "let", "of"]

-- | Insert braces at the begining of each @where, do, let, of@ that is not followed by the token
-- @TSpec _ '{'@
insertBraces :: [Token] -> [Token]
insertBraces [TIdent l1 s] | needBrace s =
  TIdent l1 s : TBrace (SLoc (-1) 0) : []
insertBraces (TIdent l1 s : TIndent l2 : TSpec _ '{' : xs) | needBrace s =
  TIdent l1 s : TIndent l2 : TSpec l2 '{' : insertBraces xs
insertBraces (TIdent l1 s : TSpec l2 '{' : xs) | needBrace s =
  TIdent l1 s : TSpec l2 '{' : insertBraces xs
insertBraces (TIdent l1 s : TIndent l : t : xs) | needBrace s =
  TIdent l1 s : TIndent l : TBrace l : insertBraces (t:xs)
insertBraces (TIdent l1 s : t : xs) | needBrace s =
  TIdent l1 s : TBrace (tokenSLoc t) : insertBraces (t:xs)
insertBraces (x:xs) = x:insertBraces xs
insertBraces [] = []

-- | Written as defined in
-- https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3
--
-- I also added two special rules to manage the case @let x = ... in@ without having to read for a
-- parsing error, instead I pop from the stack as soon as I read a @in@ (maybe preceded by an
-- indeitation symbol) in the context of a non-null open brace (generated by the corresponding
-- @let@). I also need to correctly pop in case I see the pattern @} in@ (some times with an
-- indentation patter in between the @}@ and the @in@)
layout :: [Token] -> [Int] -> [Token]
layout (TIndent n:TIdent _ "in":ts) (m:ms) | column n < m =
  TSpec n '}' : TIdent n "in" : layout ts ms
layout (TIdent n "in":ts) (m:ms) | m /= 0      = TSpec n '}' : TIdent n "in" : layout ts ms
layout (TSpec n '}':TIdent m "in":ts) (0:ms)   = TSpec n '}' : TIdent m "in" : layout ts ms
layout (TSpec n '}':TIndent m:TIdent _ "in":ts) (0:ms)   =
  TSpec n '}' : TIdent m "in" : layout ts ms

layout (TIndent n  :ts) (m:ms) | m == column n = TSpec n ';' : layout ts (m:ms)
layout (TIndent n  :ts) (m:ms) | column n < m  = TSpec n '}' : layout (TIndent n:ts) ms
layout (TIndent _  :ts) ms                     = layout ts ms

layout (TBrace  n  :ts) (m:ms) | column n > m  = TSpec n '{' : layout ts (column n:m:ms)
layout (TBrace  n  :ts) []                     = TSpec n '{' : layout ts [column n]
layout (TBrace  n  :ts) ms                     = TSpec n '{' : TSpec n '}' : layout (TBrace n:ts) ms

layout (TSpec n '}':ts) (0:ms)                 = TSpec n '}' : layout ts ms
layout (TSpec n '}':_ ) _                      = [TError n "layout error"]
layout (TSpec n '{':ts) ms                     = TSpec n '{' : layout ts (0:ms)
layout (TError n s :ts) (m:ms) | m /= 0        = TSpec n '}' : layout (TError n s:ts) ms

layout (t:ts          ) ms                     = t:layout ts ms
layout []               []                     = []
layout []               (m:ms) | m /= 0        = TSpec (SLoc 0 0) '}' : layout [] ms
layout []               _                      = [TError (SLoc (-1) 0) "layout error"]

-- | Lexing of a program
lexer :: String -> [Token]
lexer program = layout (lexed) []
  where
    lexed = insertBraces $ lexerNoLayout (SLoc 1 1) program
