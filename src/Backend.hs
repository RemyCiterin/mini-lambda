{- |
  This module is in charge of compiling a set of function lowered using @Lower.lowerDecls@ into a
  string representing a set of C function declarations/implementations and macros.

  It use a DSL with the monad @CGen@ to build C declarations, assignations, scopes...
-}

module Backend where

import AST

import qualified Data.Map as M
import Data.List (intersperse)
import Control.Monad

-- | Monad used to generate the content of a C file
data CGen a = CGen { runCGen :: [String] -> Int -> ([String], Int, a) }

instance Monad CGen where
  m >>= f = CGen (\ s1 i -> let (s2, j, a) = runCGen m s1 i in runCGen (f a) s2 j)
  return = pure

instance Functor CGen where
  fmap = liftM

instance Applicative CGen where
  pure a = CGen (\ s i -> (s, i, a))
  (<*>) = ap

-- | Generate a fresh integer
freshInt :: CGen Int
freshInt = CGen (\ s i -> (s, i+1, i))

-- | Generate a fresh name for a local variable in a C file
fresh :: CGen String
fresh = CGen (\ s i -> (s, i+1, "anon" ++ show i))

-- | Generate a list of fresh names for local variables in a C file
freshList :: Int -> CGen [String]
freshList 0 = return []
freshList n = do
  x <- fresh
  xs <- freshList (n-1)
  return (x:xs)

-- | Insert a line in a C file
insert :: String -> CGen ()
insert new = CGen (\ s i -> (new:s, i, ()))

-- | @assign "x" "y"@ will add a line of the form @"x = y;"@ in a C file
assign :: String -> String -> CGen ()
assign var val = insert $ var ++ " = " ++ val ++ ";"

-- | Same as @assign@ but for a list of assignations
assignList :: [(String, String)] -> CGen ()
assignList [] = return ()
assignList ((x,y):xs) = do
  assign x y
  assignList xs

-- | Generate a fresh C variable of type @word_t@ and declare it in the file, as example it may add
-- a line of the form @word_t anon42;@ in the file
declare :: CGen String
declare = do
  x <- fresh
  insert $ "word_t " ++ x ++ ";"
  return x

-- | Generate a fresh variable of type @word_t@, declare it in the file, and assign it, as example
-- @declareAs "53"@ may add @word_t anon42 = 53;@ in the file
declareAs :: String -> CGen String
declareAs s = do
  x <- fresh
  insert $ "word_t " ++ x ++ " = " ++ s ++ ";"
  return x

-- | Same as declare but return a list of variables instead of one
declareList :: Int -> CGen [String]
declareList 0 = return []
declareList n = do
  x <- declare
  xs <- declareList (n-1)
  return (x:xs)

-- | Wrap a set of generated lines of code in a scope, as example:
--
-- > var <- declare
-- > scope $ do
-- >  assign var "foo()"
--
-- may generate the following C code (depending of the value of the fresh variables counter):
--
-- > word_t anon42;
-- > {
-- >   anon42 = foo();
-- > }
scope :: CGen a -> CGen a
scope code = CGen (\ s1 i ->
  let (s2, j, x) = runCGen code [] i in ("}":map ("  "++) s2 ++ ("{":s1), j, x) )

-- | Generate the C code associated to a given function
implementFun :: Id -> [Id] -> Exp -> CGen ()
implementFun fun args body = do
  insert $ "// arity: "++show (length args)
  insert $ "word_t "++fun++"(word_t* args)"
  scope $ do
    vars <- declareList (length args)
    assignList (zip vars args_names)
    ret <- compileExp (M.fromList (zip args vars)) body
    insert $ "return "++ret++";"
  insert "\n"
  where
    args_names = map (\ i -> "args["++show i++"]") [0..]

-- | Generate the C code associated to a given constructor name (constructor/destructor...)
implementCons :: Id -> Int -> CGen ()
implementCons cons n = do
  uniq <- freshInt
  insert $ "#define " ++ globalConsTag cons ++ " " ++ show uniq
  insert $ "word_t "++globalConsFn cons++"(word_t* args)"
  scope $ do
    ret <- declare
    assign ret ("constructor_to_word(make_constructor("++globalConsTag cons++",args,"++show n++"))")
    insert $ "return " ++ ret ++ ";"
  insert $ "word_t "++globalConsTest cons++"(word_t* args)"
  scope $ do
    ret <- declare
    assign ret ("int_to_word(test_constructor(*args,"++globalConsTag cons++"))")
    insert $ "return " ++ ret ++ ";"

-- | Compile a set of declarations to C
compileDecls :: Decls -> CGen ()
compileDecls decls = do
  iterDecls \ name decl ->
    case decl of
      FunDecl _ _ -> insert $ "word_t "++name++"(word_t*);\n"
      ConstructorDecl nargs -> implementCons name nargs
  iterDecls \ name decl ->
    case decl of
      FunDecl args body -> implementFun name args body
      _ -> pure ()
  where
    iterDecls :: (Id -> DeclBody -> CGen ()) -> CGen ()
    iterDecls f = forM_ (M.toList decls) \ (name, decl) -> f name decl

-- | Compile an expression to C using an environment that map local variables to their names in the
-- C file.
compileExp :: (M.Map Id Id) -> Exp -> CGen String
compileExp env (EVar v) = return (env M.! v)
compileExp _ (ELit i) = do
  var <- declareAs ("int_to_word("++show i++")")
  return var

compileExp env (EAnnot e _) = compileExp env e

compileExp _ (ELambda _ _) =
  error "lambda are not supported at this stage of the compilation"

compileExp _ (ESymbol (Fun f 0)) = do
  var <- fresh
  insert $ "word_t "++var++" = "++f++"(NULL);"
  return var

compileExp _ (ESymbol (ConstructorMk _)) =
  error "constructors must be lower to C functions at this stage"

compileExp _ (ESymbol ConstructorExtract) =
  error "constructors must be lower to C functions at this stage"

compileExp _ (ESymbol (ConstructorTest _)) =
  error "constructors must be lower to C functions at this stage"

compileExp _ (ESymbol (Fun f n)) = do
  var <- declare
  assign var ("closure_to_word(make_closure((word_t)"++f++","++show n++",0))")
  return var

compileExp env (ELetIn x e1 e2) = do
  var <- compileExp env e1
  compileExp (M.insert x var env) e2

compileExp env (EApply fun []) = compileExp env fun

compileExp env (EApply (ESymbol (Fun f n)) es) | n == length es = do
  vs <- mapM (compileExp env) es
  buf <- fresh
  insert $ "word_t "++buf++"["++show n++"] = { " ++ concat (intersperse "," vs) ++ " };"
  var <- declareAs $ f++"("++buf++")"
  return var

compileExp env (EApply fun es) = do
  let m = length es
  f <- compileExp env fun
  vs <- mapM (compileExp env) es
  buf <- fresh
  insert $ "word_t "++buf++"["++show m++"] = { " ++ concat (intersperse "," vs) ++ " };"
  var <- declareAs $ "apply_closure(word_to_closure("++f++"),"++buf++","++show m++")"
  return var

compileExp env (EIte i_exp t_exp e_exp) = do
  var_i <- compileExp env i_exp
  var <- declare
  insert $ "if (word_to_int(" ++ var_i ++ "))"
  scope $ do
    var_t <- compileExp env t_exp
    assign var var_t
  insert $ "else"
  scope $ do
    var_e <- compileExp env e_exp
    assign var var_e
  return var
