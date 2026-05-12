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
import Data.Generics.Uniplate.Data

import System.IO.Unsafe

-- | Replace the global variables occurences and ADT related functions (constructors,
-- destructors...) by C function calls
createFunNodes :: Decls -> Exp -> Exp
createFunNodes decls = transform go
  where
    go (ESymbol (ConstructorMk cons)) =
      case decls M.! cons of
        ConstructorDecl arity -> ESymbol (Fun (globalConsFn cons) arity)
        _ -> error "createFunNodes"
    go (ESymbol (ConstructorTest cons)) =
      ESymbol (Fun (globalConsTest cons) 1)
    go (ESymbol ConstructorExtract) =
      ESymbol (Fun "extract_constructor" 2)
    go (EVar v) =
     case M.lookup v decls of
       Just (FunDecl args _) -> ESymbol (Fun ("fn_"++v) (length args))
       Just (ConstructorDecl _) -> error "createFunNodes"
       Nothing -> EVar v
    go e = e

-- | Optimize nested lambda abstractions and applications
simplifyLambdas :: Exp -> Exp
simplifyLambdas = transform f
  where
    f (EApply e []) = e
    f (ELambda [] e) = e
    f (ELambda l1 (ELambda l2 e)) = ELambda (l1++l2) e
    f (EApply (EApply e l1) l2) = EApply e (l1++l2)
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

permutExp :: String -> Exp -> Exp -> Exp
permutExp s e = transform go
  where
    go (EVar v) | s == v = e
    go e' = e'

-- | Lower an expression by removing all it's lambda abstractions
lowerExp :: Exp -> Closures Exp
lowerExp = transformM go
  where
    --go (ELetIn x e1 e2) | S.member x (freeVars e1) = do
    --  -- return a function taking @x@ as argument and returning the content of e1
    --  from_x <- lambda [x] e1
    --  return $ ELetIn x (EApply (ESymbol (Fun "fn_turing" 1)) [from_x]) e2
    --  -- new_fun <- fresh
    --  -- let free = S.toList (S.delete x (freeVars e1))
    --  -- let new_body =
    --  --       if length free == 0
    --  --       then ESymbol (Fun new_fun (length free))
    --  --       else EApply (ESymbol (Fun new_fun (length free))) (map EVar free)
    --  -- addClosure new_fun free (permutExp x new_body (printAndReturn "e: " True e1))
    --  -- return (ELetIn x new_body e2)
    go e@(ELambda args body) = do
      new_fun <- fresh
      let free = S.toList (freeVars e)
      addClosure new_fun (free ++ args) body
      let new_body = (EApply (ESymbol (Fun new_fun (length free + length args))) (map EVar free))
      return $ snd (printAndReturn "old/new: " True (body, new_body))
    go e = return e

    lambda args body = do
      new_fun <- fresh
      let free = S.toList (freeVars (ELambda args body))
      addClosure new_fun (free ++ args) body
      if length free == 0
      then pure $ ESymbol (Fun new_fun (length args))
      else pure $ EApply (ESymbol (Fun new_fun (length free + length args))) (map EVar free)

{-# NOINLINE printAndReturn #-}
printAndReturn :: Show a => String -> Bool -> a -> a
printAndReturn s b a = unsafePerformIO $ do
  if b then putStr s >> print a else pure ()
  return a

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
      -- let new_body' = if name == "arange" then printAndReturn new_body else new_body
      return $ M.insert ("fn_"++name) (FunDecl args new_body) new_decls
