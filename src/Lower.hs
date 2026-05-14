{- |
    Lower the lambda calculous into a lower level representation before generating C.
    This module perform some optimisations (like compressing nested function applications and lambda
    abstraction. Then it
    - rewrite all the lambda abstractions by explicit closures
    - remove all the free variables of global function declarations and replace them by
      symbols of their associated C functions.
    - rewrite all the ADT related symbols by call to C functions.
-}

module Lower(lowerDecls) where

import AST
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Uniplate

-- | Replace the global variables occurences and ADT related functions (constructors,
-- destructors...) by C function calls
createFunNodes :: Decls -> Exp -> Exp
createFunNodes = go
  where
    go decls (Symbol (ConstructorMk cons)) =
      case decls M.! cons of
        ConstructorDecl arity -> Lit (CFun (globalConsFn cons) arity)
        _ -> error "createFunNodes"
    go _ (Symbol (ConstructorTest cons)) =
      Lit (CFun (globalConsTest cons) 1)
    go _ (Symbol ConstructorTag) =
      Lit (CFun "tag_constructor" 1)
    go _ (Symbol ConstructorExtract) =
      Lit (CFun "extract_constructor" 2)
    go decls (Var v) =
     case M.lookup v decls of
       Just (FunDecl args _) -> Lit (CFun ("fn_"++v) (length args))
       Just (ConstructorDecl _) -> error "createFunNodes"
       Nothing -> Var v
    go decls (Lambda xs e) = Lambda xs (go (foldr M.delete decls xs) e)
    go decls (Apply f args) = Apply (go decls f) (map (go decls) args)
    go decls (LetIn x e1 e2) = let d = M.delete x decls in LetIn x (go d e1) (go d e2)
    go decls (Switch c l) = Switch (go decls c) (map (\ (a,b) -> (a,go decls b)) l)
    go decls (Annot e t) = Annot (go decls e) t
    go _ (Symbol s) = Symbol s
    go _ (Lit i) = Lit i

-- | Optimize nested lambda abstractions and applications
simplifyLambdas :: Exp -> Exp
simplifyLambdas = transform f
  where
    f (Apply e []) = e
    f (Lambda [] e) = e
    f (Lambda l1 (Lambda l2 e)) = Lambda (l1++l2) e
    f (Apply (Apply e l1) l2) = Apply e (l1++l2)
    f x = x

-- | A monad to create explicit closures calls and rewrite lambda abstractions
data Closures a = Closures { runClosures :: Decls -> Int -> (Decls, Int, a) }

instance Monad Closures where
  m >>= f = Closures (\ s1 i -> let (s2, j, a) = runClosures m s1 i in runClosures (f a) s2 j)
  return = pure

instance Functor Closures where
  fmap = liftM

instance Applicative Closures where
  pure a = Closures (\ s i -> (s, i, a))
  (<*>) = ap

-- | Return a fresh C function name
fresh :: Closures String
fresh = Closures (\ s i -> (s, i+1, "fun" ++ show i))

-- | Add a new closure into tha DB
addClosure :: Id -> [Id] -> Exp -> Closures ()
addClosure ident args body = Closures (\ s i -> (M.insert ident (FunDecl args body) s, i, ()))

-- | Lower an expression by removing all it's lambda abstractions, it also remove all the recursive
-- let-in
lowerExp :: Exp -> Closures Exp
lowerExp expr = transformM go1 expr >>= transformM go2
  where
    go1 (LetIn f (Lambda args@(_:_) e1) e2) | S.member f (freeVars (Lambda args e1)) = do
      new_fun <- fresh
      let free = S.toList (S.delete f (freeVars (Lambda args e1)))
      let symbol = Lit (CFun new_fun (length free + length args))
      let apply = if length free == 0 then symbol else Apply symbol (map Var free)
      let body = permute (M.singleton f apply) e1
      addClosure new_fun (free ++ args) body
      pure $ LetIn f apply e2
    go1 (LetIn f e _) | S.member f (freeVars e) = do
      error "recursive let-in must not be encapsulated in lambda abstractions"
    go1 e = return e

    go2 e@(Lambda args body) = do
      new_fun <- fresh
      let free = S.toList (freeVars e)
      addClosure new_fun (free ++ args) body
      if length free == 0
      then pure $ Lit (CFun new_fun (length args))
      else pure $ Apply (Lit (CFun new_fun (length free + length args))) (map Var free)
    go2 e = return e

-- | Perform all the previously explained optimisations and lowering phases on a set of global
-- function declarations
lowerDecls :: Decls -> Decls
lowerDecls decls =
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
      new_body <- lowerExp (simplifyLambdas (createFunNodes decls body))
      return $ M.insert ("fn_"++name) (FunDecl args (simplifyLambdas new_body)) new_decls
