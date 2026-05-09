module Backend where

import AST

import qualified Data.Map as M
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

compileFun :: Decls -> Id -> CGen ()
compileFun decls fun = do
  insert $ "word_t "++fun++"(word_t* args)"
  scope $ do
    vars <- declareList (length args)
    assignList (zip vars args_names)
    ret <- compileExp (M.fromList (zip args vars)) body
    insert $ "return "++ret++";"
  insert "\n"
  where
    (args, body) = decls M.! fun
    args_names = map (\ i -> "args["++show i++"]") [0..]

compileDecls :: Decls -> CGen ()
compileDecls decls = do
  forM_ (M.keys decls) \ fun_name -> insert $ "word_t "++fun_name++"(word_t*);\n"
  forM_ (M.keys decls) \ fun_name -> compileFun decls fun_name

compileExp :: (M.Map Id Id) -> Exp -> CGen String
compileExp env (Var v) = return (env M.! v)
compileExp _ (Const i) = do
  var <- declare
  assign var ("int_to_word("++show i++")")
  return var

compileExp _ (Lambda _ _) =
  error "lambda are not supported at this stage of the compilation"

compileExp _ (Fun f n) = do
  var <- declare
  assign var ("closure_to_word(make_closure((word_t)"++f++","++show n++",0))")
  return var

compileExp env (LetIn x e1 e2) = do
  var <- compileExp env e1
  compileExp (M.insert x var env) e2

compileExp env (Apply fun []) = compileExp env fun

compileExp env (Apply (Fun f n) es) | n == length es = do
  vs <- mapM (compileExp env) es
  var <- declare
  scope $ do
    insert $ "word_t* buf = alloca(sizeof(word_t)*"++show n++");"
    forM_ (zip [0..] vs) \ (i,v) -> assign ("buf["++show i++"]") v
    insert $ "word_t (*fun)(word_t*) = (word_t (*)(word_t*))"++f++";"
    assign var "fun(buf)"
  return var

compileExp env (Apply fun es) = do
  let m = length es
  f <- compileExp env fun
  vs <- mapM (compileExp env) es
  var <- declare
  scope $ do
    insert $ "word_t* buf = alloca(sizeof(word_t)*"++show m++");"
    forM_ (zip [0..] vs) \ (i,v) -> assign ("buf["++show i++"]") v
    assign var ("apply_closure(word_to_closure("++f++"),buf,"++show m++")")
  return var

compileExp env (Ite i_exp t_exp e_exp) = do
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
