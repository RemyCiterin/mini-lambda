module Closures(compileDecls) where

import AST
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

-- After parsing the global and local variables must be distinguished, to do so we need a pass that
-- create the `Fun` nodes and looking for the arity of the global variables
createFunNodes :: Decls -> Exp -> Exp
createFunNodes _ e@(ELit _) = e
createFunNodes decls (ESymbol (ConstructorMk cons)) =
  case decls M.! cons of
    ConstructorDecl arity -> ESymbol (Fun (globalConsFn cons) arity)
    _ -> error ""
createFunNodes _ (ESymbol (ConstructorTest cons)) =
    ESymbol (Fun (globalConsTest cons) 1)
createFunNodes _ (ESymbol (ConstructorExtract)) =
  ESymbol (Fun "extract_constructor" 2)
createFunNodes _ e@(ESymbol (Fun _ _)) = e
createFunNodes decls (EAnnot e t) = EAnnot (createFunNodes decls e) t
createFunNodes decls (EVar v) =
  case M.lookup v decls of
    Just (FunDecl args _) -> ESymbol (Fun ("fn_"++v) (length args))
    Just (ConstructorDecl _) -> error "createFunNodes"
    Nothing -> EVar v
createFunNodes decls (EIte i t e) =
  EIte (createFunNodes decls i) (createFunNodes decls t) (createFunNodes decls e)
createFunNodes decls (EApply f args) =
  EApply (createFunNodes decls f) (map (createFunNodes decls) args)
createFunNodes decls (ELetIn x e1 e2) =
  ELetIn x (createFunNodes decls e1) (createFunNodes (M.delete x decls) e2)
createFunNodes decls (ELambda args e) =
  ELambda args (createFunNodes (foldr M.delete decls args) e)

simplifyLambdas :: Exp -> Exp
simplifyLambdas (ELambda l1 (ELambda l2 e)) = simplifyLambdas (ELambda (l1++l2) e)
simplifyLambdas (ELambda l e) = ELambda l (simplifyLambdas e)
simplifyLambdas (EIte i t e) = EIte (simplifyLambdas i) (simplifyLambdas t) (simplifyLambdas e)
simplifyLambdas (EApply (EApply f l1) l2) = simplifyLambdas (EApply f (l1++l2))
simplifyLambdas (EApply f l) = EApply (simplifyLambdas f) (map simplifyLambdas l)
simplifyLambdas (ELetIn x e1 e2) = ELetIn x (simplifyLambdas e1) (simplifyLambdas e2)
simplifyLambdas (EAnnot e t) = EAnnot (simplifyLambdas e) t
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
addClosure ident args body = Closures (\ s i -> (M.insert ident (FunDecl args body) s, i, ()))

compileExp :: Exp -> Closures Exp
compileExp e@(ELit _) = pure e
compileExp e@(EAnnot _ _) = pure e
compileExp e@(EVar _) = pure e
compileExp e@(ESymbol _) = pure e
compileExp (EIte i t e) = do
  new_i <- compileExp i
  new_t <- compileExp t
  new_e <- compileExp e
  return (EIte new_i new_t new_e)
compileExp (ELetIn x e1 e2) = do
  new_e1 <- compileExp e1
  new_e2 <- compileExp e2
  return (ELetIn x new_e1 new_e2)
compileExp (EApply fun args) = do
  new_args <- mapM compileExp args
  new_fun <- compileExp fun
  return (EApply new_fun new_args)
compileExp e@(ELambda args body) = do
  let free = S.toList (freeVars e)
  new_fun <- fresh
  addClosure new_fun (free ++ args) body
  return (ESymbol (Fun new_fun (length free + length args)))

compileDecls :: Decls -> Decls
compileDecls decls =
  let (closures, _, funs) = runClosures (go (M.toList decls)) M.empty 0 in
  M.union closures funs
  where
    go :: [(Id,DeclBody)] -> Closures Decls
    go [] = return M.empty
    go ((name, d@(ConstructorDecl _)):other_decls) = do
      new_decls <- go other_decls
      return $ M.insert name d new_decls
    go ((name, FunDecl args body):other_decls) = do
      new_decls <- go other_decls
      new_body <- compileExp (simplifyLambdas (createFunNodes decls body))
      return $ M.insert ("fn_"++name) (FunDecl args new_body) new_decls
