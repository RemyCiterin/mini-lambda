module AST where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

type Id = String

data Exp
  = Const Int
  | Var Id
  | Fun Id Int
  | Ite Exp Exp Exp
  | Apply Exp [Exp]
  | LetIn Id Exp Exp
  | Lambda [Id] Exp
  deriving(Eq)

data Decl
  = ImportDecl Id
  | FunDecl Id [Exp] Exp

-- Map function names to expressions
type Decls = M.Map Id ([Id], Exp)

instance Show Exp where
  show (Const i) = show i
  show (Var ident) = ident
  show (Fun ident arity) = ident ++ "@" ++ show arity
  show (Lambda args e) = "( \\ " ++ concat (intersperse " " args) ++ " -> " ++ show e ++ ")"
  show (Ite i t e) = "( if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
  show (Apply f args) = show f ++ "(" ++ concat (intersperse "," (map show args)) ++ ")"
  show (LetIn x e1 e2) = "( let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

freeVars :: Exp -> S.Set Id
freeVars (Const _) = S.empty
freeVars (Var x) = S.singleton x
freeVars (Fun _ _) = S.empty
freeVars (Ite i t e) = S.union (freeVars i) (S.union (freeVars t) (freeVars e))
freeVars (Apply fun (x:xs)) =
  S.union (freeVars x) (freeVars (Apply fun xs))
freeVars (Apply fun []) = freeVars fun
freeVars (LetIn x e1 e2) =
  S.union (freeVars e1) (S.delete x (freeVars e2))
freeVars (Lambda [] e) = freeVars e
freeVars (Lambda (x:xs) e) =
  S.delete x (freeVars (Lambda xs e))

