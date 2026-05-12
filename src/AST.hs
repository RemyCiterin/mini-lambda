{- |

-}

module AST where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Data

-- | Identifier, their is no difference between qualified and unqualified identifiers yet
type Id = String

-- | Symbols and global variables are used to represent pointers to C functions and ADT related
-- functions (constructors/destructors)
data Symbol
  = ConstructorExtract  -- ^ Extract a field of a constructor
  | ConstructorTest Id  -- ^ Test the tag of a constructor
  | ConstructorMk Id    -- ^ Build a constructor
  | Fun Id Int          -- ^ C function and the length of the arguments buffer
  deriving(Eq, Show, Typeable, Data)

-- | Expressions
data Exp
  = ELit Int            -- ^ Constant integer
  | EVar Id             -- ^ Local variable or function call (before lowering)
  | ESymbol Symbol      -- ^ Known global symbol (C function, constructor, destructor...)
  | EIte Exp Exp Exp    -- ^ If Then Else
  | EApply Exp [Exp]    -- ^ Function application
  | ELetIn Id Exp Exp   -- ^ Local variable definition
  | ELambda [Id] Exp    -- ^ Lambda abstraction
  | EAnnot Exp Type     -- ^ Type annotation
  deriving(Eq, Typeable, Data)

-- | General type
type Sigma = Type

-- | Non top-level @TForall@ type
type Rho = Type

-- | A type without @TForall@
type Tau = Type

-- | Types definition
data Type
  = TForAll [Id] Rho
  -- ^ Type abstraction
  | TFun Type Type
  -- ^ Type of functions
  | TInt
  -- ^ Integer type
  deriving(Eq, Show, Typeable, Data)

-- | Type variables
data TyVar
  = BoundTv String
  -- ^ A type variable bounded by a @Forall@
  | SlokemTv String Uniq
  -- ^ A skolemised type variable, used to generate fresh type variables
  deriving(Eq, Show, Typeable, Data)

-- | Unique identifier of a slolemised variable
type Uniq = Int

-- | A declaration is either a function declaration, or the declaration of a constructor of a given
-- arity
data DeclBody
  = FunDecl [Id] Exp
  -- ^ Declare a function with a given set of arguments and a body
  | ConstructorDecl Int
  -- ^ Declare a constructor of a specific arity

-- | Map function names to expressions
type Decls = M.Map Id DeclBody

-- | Return @True@ if a function declaration is a function
declIsFun :: DeclBody -> Bool
declIsFun (FunDecl _ _) = True
declIsFun _ = False

-- | Return @True@ if a declaration refer to a constructor
declIsCons :: DeclBody -> Bool
declIsCons (ConstructorDecl _) = True
declIsCons _ = False

-- | Return the name of the C function generated to build a constructor
globalConsFn :: Id -> Id
globalConsFn cons = "cons_" ++ cons

-- | Return the name of the C function generated to test the tag of an ADT element
globalConsTest :: Id -> Id
globalConsTest cons = "test_cons_" ++ cons

-- | Return the name of the C function generated to extract the fields of a constructor/record
globalConsExtract :: Id
globalConsExtract = "extract_constructor"

-- | Return the defined C macro to read the tag of a constructor/record
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

-- | Return the free variables in an expression
freeVars :: Exp -> S.Set Id
freeVars (ELit _) = S.empty
freeVars (EVar x) = S.singleton x
freeVars (ESymbol _) = S.empty
freeVars (EIte i t e) = S.union (freeVars i) (S.union (freeVars t) (freeVars e))
freeVars (EApply fun (x:xs)) =
  S.union (freeVars x) (freeVars (EApply fun xs))
freeVars (EApply fun []) = freeVars fun
freeVars (ELetIn x e1 e2) =
  S.union (S.delete x (freeVars e1)) (S.delete x (freeVars e2))
freeVars (ELambda [] e) = freeVars e
freeVars (ELambda (x:xs) e) =
  S.delete x (freeVars (ELambda xs e))
freeVars (EAnnot e _) = freeVars e

permute :: (M.Map Id Exp) -> Exp -> Exp
permute _ e@(ELit _) = e
permute m e@(EVar i) =
  case M.lookup i m of
    Just v -> v
    _ -> e
permute _ e@(ESymbol _) = e
permute m (EIte i t e) = EIte (permute m i) (permute m t) (permute m e)
permute m (EApply f a) = EApply (permute m f) (map (permute m) a)
permute m (EAnnot e t) = EAnnot (permute m e) t
permute m (ELambda xs e) = ELambda xs $ permute (foldr M.delete m xs) e
permute m (ELetIn x e1 e2) = let m' = M.delete x m in ELetIn x (permute m' e1) (permute m' e2)
