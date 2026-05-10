module AST where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

type Id = String

data Symbol
  = ConstructorExtract  -- Extract a field of a constructor
  | ConstructorTest Id  -- Test the tag of a constructor
  | ConstructorMk Id    -- Build a constructor
  | Fun Id Int          -- C function and the length of the arguments buffer
  deriving(Eq, Show)

data Exp
  = ELit Int
  | EVar Id
  | ESymbol Symbol
  | EIte Exp Exp Exp
  | EApply Exp [Exp]
  | ELetIn Id Exp Exp
  | ELambda [Id] Exp
  | EAnnot Exp Type
  deriving(Eq)

type Sigma = Type
type Rho = Type
type Tau = Type

data Type
  = TForAll [Id] Rho
  | TFun Type Type
  | TInt
  deriving(Eq, Show)

data TyVar
  = BoundTv String
  | SlokemTv String Uniq
  deriving(Eq, Show)

type Uniq = Int

data DeclBody
  = FunDecl [Id] Exp
  | ConstructorDecl Int

-- Map function names to expressions
type Decls = M.Map Id DeclBody

declIsFun :: DeclBody -> Bool
declIsFun (FunDecl _ _) = True
declIsFun _ = False

declIsCons :: DeclBody -> Bool
declIsCons (ConstructorDecl _) = True
declIsCons _ = False

globalConsFn :: Id -> Id
globalConsFn cons = "cons_" ++ cons

globalConsTest :: Id -> Id
globalConsTest cons = "test_cons_" ++ cons

globalConsExtract :: Id -> Int -> Id
globalConsExtract cons idx = "extract" ++ show idx ++ "_cons_" ++ cons

globalConsTag :: Id -> Id
globalConsTag cons = "CONS_" ++ cons

instance Show Exp where
  show (ELit i) = show i
  show (EVar ident) = ident
  show (ESymbol ident) = show ident
  show (ELambda args e) = "( \\ " ++ concat (intersperse " " args) ++ " -> " ++ show e ++ ")"
  show (EIte i t e) = "( if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
  show (EApply f args) = show f ++ "(" ++ concat (intersperse "," (map show args)) ++ ")"
  show (ELetIn x e1 e2) = "( let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
  show (EAnnot e t) = "( " ++ show e ++ " :: " ++ show t ++ " )"

instance Show DeclBody where
  show (FunDecl args body) = concat (intersperse " " args) ++ " = " ++ show body
  show (ConstructorDecl args) = show "tuple("++show args++")"

freeVars :: Exp -> S.Set Id
freeVars (ELit _) = S.empty
freeVars (EVar x) = S.singleton x
freeVars (ESymbol _) = S.empty
freeVars (EIte i t e) = S.union (freeVars i) (S.union (freeVars t) (freeVars e))
freeVars (EApply fun (x:xs)) =
  S.union (freeVars x) (freeVars (EApply fun xs))
freeVars (EApply fun []) = freeVars fun
freeVars (ELetIn x e1 e2) =
  S.union (freeVars e1) (S.delete x (freeVars e2))
freeVars (ELambda [] e) = freeVars e
freeVars (ELambda (x:xs) e) =
  S.delete x (freeVars (ELambda xs e))
freeVars (EAnnot e _) = freeVars e
