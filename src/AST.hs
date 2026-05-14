{- |

-}

module AST where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Uniplate

import Data.IORef

import Lexer(SLoc(..))

-- | Identifier, their is no difference between qualified and unqualified identifiers yet
type Id = String

-- | Symbols and global variables are used to represent pointers to C functions and ADT related
-- functions (constructors/destructors)
data Symbol
  = ConstructorExtract  -- ^ Extract a field of a constructor
  | ConstructorTest Id  -- ^ Test the tag of a constructor
  | ConstructorTag      -- ^ Return the tag of a constructor
  | ConstructorMk Id    -- ^ Build a constructor

-- | Expressions
data Exp
  = Lit Lit                    -- ^ Constant integer
  | Var Id                     -- ^ Local variable or function call (before lowering)
  | Symbol Symbol              -- ^ Known global symbol (C function, constructor, destructor...)
  | Switch Exp [(Lit, Exp)]    -- ^ switch (used to implement pattern matching and if-then-else)
  | Apply Exp [Exp]            -- ^ Function application
  | LetIn Id Exp Exp           -- ^ Local variable definition
  | Lambda [Id] Exp            -- ^ Lambda abstraction
  | Annot Exp Type             -- ^ Type annotation
--   | Cons Id                    -- ^ Constructor
--   | Case Exp [(Pattern, Exp)]  -- ^ Pattern matching

data Pattern
  = Wildcard
  | Constructor Id [Pattern]
  | PVar Id

-- | Builtin expressions
data Lit
  = Int Int          -- ^ A fixed integer
  | Tag String       -- ^ A constructor tag: of type integer
  | CFun String Int  -- ^ A C function of a given arity
  | Undefined        -- ^ An undefined object

-- | General type
type Sigma = Type

-- | Non top-level @Forall@ type
type Rho = Type

-- | A type without @Forall@
type Tau = Type

-- | Types definition
data Type
  = Forall [TVar] Rho
  -- ^ Type abstraction
  | Arrow Type Type
  -- ^ Type of functions
  | TInt
  -- ^ Integer type
  | TVar TVar
  -- ^ Type variable
  | MVar MVar
  -- ^ Meta type variable, created by the type checker

data Kind
  = Star
  | KArrow Kind Kind

-- | Type variables
data TVar
  = BoundTv Id
  -- ^ A type variable bounded by a @Forall@
  | SkolemTv Id Uniq
  -- ^ A skolemised type variable, used to generate fresh type variables

-- | Unique identifier of a slolemised variable
type Uniq = Int

-- | MetaType variable created by the type checker
data MVar = Meta Uniq TyRef

-- | Nothing means that the variable has not been substitued yet
type TyRef = IORef (Maybe Tau)

-- | A declaration is either a function declaration, or the declaration of a constructor of a given
-- arity
data DeclBody
  = FunDecl [Id] Exp
  -- ^ Declare a function with a given set of arguments and a body
  | ConstructorDecl Int
  -- ^ Declare a constructor of a specific arity

-- | Map function names to expressions
type Decls = M.Map Id DeclBody

instance Eq MVar where
  (Meta u1 _) == (Meta u2 _) = u1 == u2

instance Eq TVar where
  (BoundTv s1) == (BoundTv s2) = s1 == s2
  (SkolemTv _ u1) == (SkolemTv _ u2) = u1 == u2
  _ == _ = False

instance Ord TVar where
  (BoundTv s1) <= (BoundTv s2) = s1 <= s2
  (SkolemTv _ u1) <= (SkolemTv _ u2) = u1 <= u2
  (SkolemTv _ _) <= (BoundTv _) = True
  _ <= _ = False

(-->) :: Sigma -> Sigma -> Sigma
arg --> res = Arrow arg res

instance Show Lit where
  show (CFun name _) = show name
  show Undefined = "undefined"
  show (Int i) = show i
  show (Tag t) = t

instance Show Symbol where
  show (ConstructorMk c) = c
  show (ConstructorTest c) = '?':c
  show ConstructorExtract = "at"
  show ConstructorTag = "tag"

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

instance Uniplate Exp where
  uniplate e@(Lit _) = ([], \ _ -> e)
  uniplate e@(Var _) = ([], \ _ -> e)
  uniplate e@(Symbol _) = ([], \ _ -> e)
  uniplate (Lambda args e) = ([e], \ l -> Lambda args (l!!0))
  uniplate (Apply f args) = (f:args, \ l -> Apply (l!!0) (drop 1 l))
  uniplate (LetIn x e1 e2) = ([e1,e2], \ l -> LetIn x (l!!0) (l!!1))
  uniplate (Annot e t) = ([e], \ l -> Annot (l!!0) t)
  uniplate (Switch cond args) =
    (cond:map snd args, \ l -> Switch (l!!0) (zip (map fst args) (drop 1 l)))

instance Show Exp where
  show (Lit i) = show i
  show (Var ident) = ident
  show (Symbol ident) = show ident
  show (Lambda args e) = "( \\ " ++ concat (intersperse " " args) ++ " -> " ++ show e ++ " )"
  show (Switch i [(Int 1,t), (Undefined, e)]) =
    "( if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ " )"
  show (Switch cond pairs) =
    "(" ++ intercalate "; "
      (map (\(val,body) -> show cond ++ " == " ++ show val ++ " -> " ++ show body) pairs) ++ ")"
  show (Apply f args) = show f ++ "(" ++ concat (intersperse "," (map show args)) ++ ")"
  show (LetIn x e1 e2) = "( let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ " )"
  show (Annot e t) = "( " ++ show e ++ " :: " ++ show t ++ " )"

instance Show DeclBody where
  show (FunDecl args body) = concat (intersperse " " args) ++ " = " ++ show body
  show (ConstructorDecl args) = show "tuple("++show args++")"

-- | Return the free variables in an expression
freeVars :: Exp -> S.Set Id
freeVars (Lit _) = S.empty
freeVars (Var x) = S.singleton x
freeVars (Symbol _) = S.empty
-- freeVars (Ite i t e) = S.union (freeVars i) (S.union (freeVars t) (freeVars e))
freeVars (Switch cond []) = freeVars cond
freeVars (Switch cond ((_,x):xs)) = S.union (freeVars x) (freeVars (Switch cond xs))
freeVars (Apply fun (x:xs)) =
  S.union (freeVars x) (freeVars (Apply fun xs))
freeVars (Apply fun []) = freeVars fun
freeVars (LetIn x e1 e2) =
  S.union (S.delete x (freeVars e1)) (S.delete x (freeVars e2))
freeVars (Lambda [] e) = freeVars e
freeVars (Lambda (x:xs) e) =
  S.delete x (freeVars (Lambda xs e))
freeVars (Annot e _) = freeVars e

permute :: (M.Map Id Exp) -> Exp -> Exp
permute _ e@(Lit _) = e
permute m e@(Var i) =
  case M.lookup i m of
    Just v -> v
    _ -> e
permute _ e@(Symbol _) = e
permute m (Switch c l) = Switch (permute m c) (map (\ (lit,val) -> (lit, permute m val)) l)
permute m (Apply f a) = Apply (permute m f) (map (permute m) a)
permute m (Annot e t) = Annot (permute m e) t
permute m (Lambda xs e) = Lambda xs $ permute (foldr M.delete m xs) e
permute m (LetIn x e1 e2) = let m' = M.delete x m in LetIn x (permute m' e1) (permute m' e2)


freeMVars :: [Type] -> [MVar]
freeMVars tys = foldr go [] tys
  where
    go (MVar tv) acc
      | tv `elem` acc     = acc
      | otherwise         = tv:acc
    go (TVar _) acc      = acc
    go TInt      acc      = acc
    go (Arrow arg res) acc = go arg (go res acc)
    go (Forall _ ty) acc = go ty acc

freeTVars :: [Type] -> [TVar]
freeTVars tys = foldr (go []) [] tys
  where
    go bound (TVar v) acc
      | v `elem` bound            = acc
      | v `elem` acc              = acc
      | otherwise                 = v:acc
    go _     TInt acc             = acc
    go _     (MVar _) acc       = acc
    go bound (Forall tvs ty) acc = go (tvs ++ bound) ty acc
    go bound (Arrow arg res) acc   = go bound arg (go bound res acc)

bindersTVar :: Rho -> [TVar]
bindersTVar ty = nub (binders ty)
  where
    binders (Forall tvs body) = tvs ++ binders body
    binders (Arrow arg res)     = binders arg ++ binders res
    binders _                  = []

type TyEnv = M.Map TVar Tau
substitute :: TyEnv -> Type -> Type
substitute env (Arrow arg res) = Arrow (substitute env arg) (substitute env res)
substitute env (TVar v) =
  case M.lookup v env of
    Just x -> x
    _ -> TVar v
substitute env (Forall tys ty) = Forall tys (substitute (foldr M.delete env tys) ty)
substitute _ t = t

tyVarId :: TVar -> Id
tyVarId (SkolemTv i _) = i
tyVarId (BoundTv i) = i


instance Show MVar where
  show (Meta u _) = "?" ++ show u

instance Show TVar where
  show (BoundTv s) = s
  show (SkolemTv s u) = s ++ "#" ++ show u

instance Show Type where
  show (Forall args body) = "forall " ++ intercalate " " (map show args) ++ ". " ++ show body
  show (Arrow arg@(Forall _ _) res) = "(" ++ show arg ++ ") -> " ++ show res
  show (Arrow arg@(Arrow _ _) res) = "(" ++ show arg ++ ") -> " ++ show res
  show (Arrow arg res) = show arg ++ " -> " ++ show res
  show (TVar tvar) = show tvar
  show (MVar mvar) = show mvar
  show TInt = "int"
