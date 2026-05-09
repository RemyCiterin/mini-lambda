module Closures(compileDecls) where

import AST
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

-- After parsing the global and local variables must be distinguished, to do so we need a pass that
-- create the `Fun` nodes and looking for the arity of the global variables
createFunNodes :: Decls -> Exp -> Exp
createFunNodes _ e@(Const _) = e
createFunNodes _ e@(Fun _ _) = e
createFunNodes decls (Var v) =
  case M.lookup v decls of
    Just (args, _) -> Fun ("fn_"++v) (length args)
    Nothing -> Var v
createFunNodes decls (Ite i t e) =
  Ite (createFunNodes decls i) (createFunNodes decls t) (createFunNodes decls e)
createFunNodes decls (Apply f args) =
  Apply (createFunNodes decls f) (map (createFunNodes decls) args)
createFunNodes decls (LetIn x e1 e2) =
  LetIn x (createFunNodes decls e1) (createFunNodes (M.delete x decls) e2)
createFunNodes decls (Lambda args e) =
  Lambda args (createFunNodes (foldr M.delete decls args) e)

simplifyLambdas :: Exp -> Exp
simplifyLambdas (Lambda l1 (Lambda l2 e)) = simplifyLambdas (Lambda (l1++l2) e)
simplifyLambdas (Lambda l e) = Lambda l (simplifyLambdas e)
simplifyLambdas (Ite i t e) = Ite (simplifyLambdas i) (simplifyLambdas t) (simplifyLambdas e)
simplifyLambdas (Apply (Apply f l1) l2) = simplifyLambdas (Apply f (l1++l2))
simplifyLambdas (Apply f l) = Apply (simplifyLambdas f) (map simplifyLambdas l)
simplifyLambdas (LetIn x e1 e2) = LetIn x (simplifyLambdas e1) (simplifyLambdas e2)
simplifyLambdas e = e

data Closures a = Closures { runClosures :: Decls -> Int -> (Decls, Int, a) }

instance Monad Closures where
  m >>= f = Closures (\ s1 i -> let (s2, j, a) = runClosures m s1 i in runClosures (f a) s2 j)
  return = pure

instance Functor Closures where
  fmap = liftM

instance Applicative Closures where
  pure a = Closures (\ s i -> (s, i, a))
  (<*>) = ap

fresh :: Closures String
fresh = Closures (\ s i -> (s, i+1, "fun" ++ show i))

addClosure :: Id -> [Id] -> Exp -> Closures ()
addClosure ident args body = Closures (\ s i -> (M.insert ident (args, body) s, i, ()))

compileExp :: Exp -> Closures Exp
compileExp e@(Const _) = pure e
compileExp e@(Var _) = pure e
compileExp e@(Fun _ _) = pure e
compileExp (Ite i t e) = do
  new_i <- compileExp i
  new_t <- compileExp t
  new_e <- compileExp e
  return (Ite new_i new_t new_e)
compileExp (LetIn x e1 e2) = do
  new_e1 <- compileExp e1
  new_e2 <- compileExp e2
  return (LetIn x new_e1 new_e2)
compileExp (Apply fun args) = do
  new_args <- mapM compileExp args
  new_fun <- compileExp fun
  return (Apply new_fun new_args)
compileExp e@(Lambda args body) = do
  let free = S.toList (freeVars e)
  new_fun <- fresh
  addClosure new_fun (free ++ args) body
  return (Fun new_fun (length free + length args))

compileDecls :: Decls -> Decls
compileDecls decls =
  let (closures, _, funs) = runClosures (go (M.toList decls)) M.empty 0 in
  M.union closures funs
  where
    go :: [(Id,([Id],Exp))] -> Closures Decls
    go [] = return M.empty
    go ((name, (args, body)):other_decls) = do
      new_decls <- go other_decls
      new_body <- compileExp (simplifyLambdas (createFunNodes decls body))
      return $ M.insert ("fn_"++name) (args, new_body) new_decls
