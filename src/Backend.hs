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
  insert $ unlines $ map ("//" ++) (lines (show body))
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
compileExp env (Var v) =
  case M.lookup v env of
    Just x -> return x
    _ -> error (v ++ " is not found in context")
compileExp _ (Lit (Int i)) = do
  var <- declareAs ("int_to_word("++show i++")")
  return var

compileExp _ (Lit (Tag i)) = do
  var <- declareAs ("int_to_word("++show i++")")
  return var

compileExp _ (Lit Undefined) = do
  var <- declare
  return var

compileExp _ (Lit (Cons _)) =
  error "those expression should be lowered to CFun nodes"

compileExp env (Annot e _) = compileExp env e

compileExp _ (Lambda _ _) =
  error "lambda are not supported at this stage of the compilation"

compileExp _ (Lit (CFun f n)) = compileClosure f n

compileExp env (LetIn x e1 e2) = do
  var <- compileExp env e1
  compileExp (M.insert x var env) e2

compileExp env (Apply fun []) = compileExp env fun

compileExp env (Apply (Lit (CFun f n)) es) | n == length es = do
  compileFullApply env f es

compileExp env (Apply fun es) = do
  let m = length es
  f <- compileExp env fun
  vs <- mapM (compileExp env) es
  buf <- fresh
  insert $ "word_t "++buf++"["++show m++"] = { " ++ concat (intersperse "," vs) ++ " };"
  var <- declareAs $ "apply_closure(word_to_closure("++f++"),"++buf++","++show m++")"
  return var

compileExp env (Switch cond list) = do
  var_i <- compileExp env cond
  var <- declare
  insert $ "switch (word_to_int(" ++ var_i ++ "))"
  scope $ do
    go var list
  return var
    where
      go var ((Undefined, expr):_) = do
        insert $ "default:"
        x <- compileExp env expr
        assign var x
        insert "break;"
      go var ((lit, expr):others) = do
        insert $ "case " ++ show lit ++":"
        x <- compileExp env expr
        assign var x
        insert "break;"
        go var others
      go _ [] = pure ()

compileExp env (Case e patterns) = do
  compileCase env e patterns

compileClosure :: String -> Int -> CGen String
compileClosure f 0 = declareAs (f++"(NULL)")
compileClosure f n = declareAs ("closure_to_word(make_closure((word_t)"++f++","++show n++",0))")

compileFullApply :: M.Map Id Id -> String -> [Exp] -> CGen String
compileFullApply env f es = do
  buf <- fresh
  vs <- mapM (compileExp env) es
  insert $ "word_t "++buf++"["++show (length es)++"] = { " ++ concat (intersperse "," vs) ++ " };"
  var <- declareAs $ f++"("++buf++")"
  return var

compileCase :: M.Map Id Id -> Exp -> [(Pattern,Exp)] -> CGen String
compileCase env expr cases = do
  var <- compileExp env expr
  tag <- declareAs ("word_to_int(tag_constructor(&"++ var ++"))")
  ret <- declare

  insert $ "switch ("++tag++")"
  scope do
    go var ret cases
  return ret
    where
      go _ _ [] = pure ()
      go _ ret ((Wildcard,e):_) = do
        insert "default:"
        x <- compileExp env e
        assign ret x
      go var ret ((PVar v,e):_) = do
        insert "default:"
        x <- compileExp (M.insert v var env) e
        assign ret x
      go var ret ((PCons name args,e):xs) = do
        insert $ "case " ++ globalConsTag name ++ ":"
        patterns <- mapM (extract var) (zip args [0..])
        compileCaseArm env patterns e ret
        go var ret xs

      extract :: Id -> (Pattern,Int) -> CGen (Pattern,Id)
      extract var (pat,i) = do
        buf <- fresh
        insert $ "word_t "++buf++"[2] = { " ++ var ++ ",int_to_word(" ++ show i ++ ") };"
        ret <- declareAs ("extract_constructor("++buf++")")
        pure (pat,ret)


compileCaseArm :: M.Map Id Id -> [(Pattern,Id)] -> Exp -> Id -> CGen ()
compileCaseArm env [] expr res = do
  x <- compileExp env expr
  assign res x
compileCaseArm env ((Wildcard,_):xs) expr res = do
  compileCaseArm env xs expr res
compileCaseArm env ((PVar v,e):xs) expr res = do
  compileCaseArm (M.insert v e env) xs expr res
compileCaseArm env ((PCons c l,e):xs) expr res = do
  tag <- declareAs ("word_to_int(&tag_constructor("++ e ++"))")
  insert $ "if (" ++ tag ++ " == " ++ globalConsTag c ++ ")"
  scope do
    patterns <- mapM (extract e) (zip l [0..])
    compileCaseArm env (patterns ++ xs) expr res
  where
    extract :: Id -> (Pattern,Int) -> CGen (Pattern,Id)
    extract var (pat,i) = do
      buf <- fresh
      insert $ "word_t "++buf++"[2] = { " ++ var ++ ",int_to_word(" ++ show i ++ ") };"
      ret <- declareAs ("extract_constructor("++buf++")")
      pure (pat,ret)
