module Backend where

import AST

import qualified Data.Map as M
import Data.List (intersperse)
import Control.Monad

data CGen a = CGen { runCGen :: [String] -> Int -> ([String], Int, a) }

instance Monad CGen where
  m >>= f = CGen (\ s1 i -> let (s2, j, a) = runCGen m s1 i in runCGen (f a) s2 j)
  return = pure

instance Functor CGen where
  fmap = liftM

instance Applicative CGen where
  pure a = CGen (\ s i -> (s, i, a))
  (<*>) = ap

freshInt :: CGen Int
freshInt = CGen (\ s i -> (s, i+1, i))

fresh :: CGen String
fresh = CGen (\ s i -> (s, i+1, "anon" ++ show i))

freshList :: Int -> CGen [String]
freshList 0 = return []
freshList n = do
  x <- fresh
  xs <- freshList (n-1)
  return (x:xs)

insert :: String -> CGen ()
insert new = CGen (\ s i -> (new:s, i, ()))

assign :: String -> String -> CGen ()
assign var val = insert $ var ++ " = " ++ val ++ ";"

assignList :: [(String, String)] -> CGen ()
assignList [] = return ()
assignList ((x,y):xs) = do
  assign x y
  assignList xs

declare :: CGen String
declare = do
  x <- fresh
  insert $ "word_t " ++ x ++ ";"
  return x

declareList :: Int -> CGen [String]
declareList 0 = return []
declareList n = do
  x <- declare
  xs <- declareList (n-1)
  return (x:xs)

scope :: CGen a -> CGen a
scope code = CGen (\ s1 i ->
  let (s2, j, x) = runCGen code [] i in ("}":map ("  "++) s2 ++ ("{":s1), j, x) )

implementFun :: Id -> [Id] -> Exp -> CGen ()
implementFun fun args body = do
  insert $ "word_t "++fun++"(word_t* args)"
  scope $ do
    vars <- declareList (length args)
    assignList (zip vars args_names)
    ret <- compileExp (M.fromList (zip args vars)) body
    insert $ "return "++ret++";"
  insert "\n"
  where
    args_names = map (\ i -> "args["++show i++"]") [0..]

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

compileExp :: (M.Map Id Id) -> Exp -> CGen String
compileExp env (EVar v) = return (env M.! v)
compileExp _ (ELit i) = do
  var <- declare
  assign var ("int_to_word("++show i++")")
  return var

compileExp env (EAnnot e _) = compileExp env e

compileExp _ (ELambda _ _) =
  error "lambda are not supported at this stage of the compilation"

compileExp _ (ESymbol (Fun f 0)) = do
  var <- declare
  assign var (f++"(NULL)")
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
  var <- declare
  scope $ do
    insert $ "word_t buf["++show n++"] = { " ++ concat (intersperse "," vs) ++ " };"
    assign var $ f++"(buf)"
  return var

compileExp env (EApply fun es) = do
  let m = length es
  f <- compileExp env fun
  vs <- mapM (compileExp env) es
  var <- declare
  scope $ do
    insert $ "word_t buf["++show m++"] = { " ++ concat (intersperse "," vs) ++ " };"
    assign var ("apply_closure(word_to_closure("++f++"),buf,"++show m++")")
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
