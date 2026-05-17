{- |

-}

module AST where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Uniplate

import Prelude hiding((<>))
import Text.PrettyPrint

import Data.IORef

import Lexer(SLoc(..))

-- | Identifier, their is no difference between qualified and unqualified identifiers yet
type Id = String

-- | Expressions
data Exp
  = Lit Lit                    -- ^ Constant integer
  | Var Id                     -- ^ Local variable or function call (before lowering)
  | Ite Exp Exp Exp            -- ^ Conditional operations
  | Apply Exp [Exp]            -- ^ Function application
  | LetIn Id Exp Exp           -- ^ Local variable definition
  | Lambda [Id] Exp            -- ^ Lambda abstraction
  | Annot Exp Type             -- ^ Type annotation
  | Case Exp [(Pattern, Exp)]  -- ^ Pattern matching

data Pattern
  = Wildcard
  | PCons Id [Pattern]
  | PVar Id
  | PInt Int
  deriving (Eq, Ord)

-- | Builtin expressions
data Lit
  = Int Int          -- ^ A fixed integer
  | Tag String       -- ^ A constructor tag: of type integer
  | CFun String Int  -- ^ A C function of a given arity
  | Undefined        -- ^ An undefined object
  | Cons Id          -- ^ A type constructor

-- | Types definition
data Type
  = Arrow Type Type
  -- ^ Type of functions
  | App Type Type
  -- ^ Type application
  | TConst String
  -- ^ Integer type
  | TVar TVar
  -- ^ Type variable
  | MVar MVar
  -- ^ Meta type variable, created by the type checker

data Scheme = Forall [TVar] Type

data Kind
  = Star
  | KArrow Kind Kind

-- | Type variables
data TVar
  = Bound Id
  -- ^ A type variable bounded by a @Forall@
  | Skolem Id Uniq
  -- ^ A skolemised type variable, used to generate fresh type variables

-- | Unique identifier of a slolemised variable
type Uniq = Int

-- | MetaType variable created by the type checker
data MVar = Meta Uniq TyRef

-- | Nothing means that the variable has not been substitued yet
type TyRef = IORef (Maybe Type)

-- | A declaration is either a function declaration, or the declaration of a constructor of a given
-- arity
data DeclBody
  = FunDecl [Id] Exp (Maybe Scheme)
  -- ^ Declare a function with a given set of arguments and a body
  | ConstructorDecl Scheme
  -- ^ Declare a constructor of a specific arity

-- | Map function names to expressions
type Decls = M.Map Id DeclBody

instance Eq MVar where
  (Meta u1 _) == (Meta u2 _) = u1 == u2

instance Eq TVar where
  (Bound s1) == (Bound s2) = s1 == s2
  (Skolem _ u1) == (Skolem _ u2) = u1 == u2
  _ == _ = False

instance Ord TVar where
  (Bound s1) <= (Bound s2) = s1 <= s2
  (Skolem _ u1) <= (Skolem _ u2) = u1 <= u2
  (Skolem _ _) <= (Bound _) = True
  _ <= _ = False

(-->) :: Type -> Type -> Type
arg --> res = Arrow arg res

instance Show Lit where
  show (CFun name _) = show name
  show (Cons name) = show name
  show Undefined = "undefined"
  show (Int i) = show i
  show (Tag t) = t

instance Show Pattern where
  show (PInt i) = show i
  show Wildcard = "_"
  show (PVar v) = v
  show (PCons name args) = "(" ++ name ++ " " ++ intercalate " " (map show args) ++ ")"

-- | Return @True@ if a function declaration is a function
declIsFun :: DeclBody -> Bool
declIsFun (FunDecl _ _ _) = True
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
  uniplate (Lambda args e) = ([e], \ l -> Lambda args (l!!0))
  uniplate (Apply f args) = (f:args, \ l -> Apply (l!!0) (drop 1 l))
  uniplate (LetIn x e1 e2) = ([e1,e2], \ l -> LetIn x (l!!0) (l!!1))
  uniplate (Annot e t) = ([e], \ l -> Annot (l!!0) t)
  uniplate (Ite i t e) =
    ([i,t,e], \ l -> Ite (l!!0) (l!!1) (l!!2))
  uniplate (Case val cases) =
    (val:map snd cases, \ l -> Case (l!!0) (zip (map fst cases) (drop 1 l)))


instance Show Exp where
  show (Lit i) = show i
  show (Var ident) = ident
  show (Lambda args e) = "( \\ " ++ concat (intersperse " " args) ++ " -> " ++ show e ++ " )"
  show (Ite i t e) =
    "( if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ " )"
  show (Apply f args) = show f ++ "(" ++ concat (intersperse "," (map show args)) ++ ")"
  show (LetIn x e1 e2) = "( let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ " )"
  show (Annot e t) = "( " ++ show e ++ " :: " ++ show t ++ " )"
  show (Case _ _) = "cases"

instance Show DeclBody where
  show (FunDecl args body _) = concat (intersperse " " args) ++ " = " ++ show body
  show (ConstructorDecl args) = show "tuple("++show args++")"

-- | Return the free variables in an expression
freeVars :: Exp -> S.Set Id
freeVars (Lit _) = S.empty
freeVars (Var x) = S.singleton x
freeVars (Ite i t e) = S.union (freeVars i) (S.union (freeVars t) (freeVars e))
freeVars (Apply fun (x:xs)) =
  S.union (freeVars x) (freeVars (Apply fun xs))
freeVars (Apply fun []) = freeVars fun
freeVars (LetIn x e1 e2) =
  S.union (S.delete x (freeVars e1)) (S.delete x (freeVars e2))
freeVars (Lambda [] e) = freeVars e
freeVars (Lambda (x:xs) e) =
  S.delete x (freeVars (Lambda xs e))
freeVars (Annot e _) = freeVars e
freeVars (Case e ((pat,val):patterns)) =
  S.union (S.difference (freeVars val) (patternVars pat)) (freeVars (Case e patterns))
freeVars (Case e []) = freeVars e


patternVars :: Pattern -> S.Set Id
patternVars Wildcard = S.empty
patternVars (PInt _) = S.empty
patternVars (PVar v) = S.singleton v
patternVars (PCons _ args) = foldr S.union S.empty (map patternVars args)

permute :: (M.Map Id Exp) -> Exp -> Exp
permute _ e@(Lit _) = e
permute m e@(Var i) =
  case M.lookup i m of
    Just v -> v
    _ -> e
permute m (Ite i t e) = Ite (permute m i) (permute m t) (permute m e)
permute m (Apply f a) = Apply (permute m f) (map (permute m) a)
permute m (Annot e t) = Annot (permute m e) t
permute m (Lambda xs e) = Lambda xs $ permute (foldr M.delete m xs) e
permute m (LetIn x e1 e2) = let m' = M.delete x m in LetIn x (permute m' e1) (permute m' e2)
permute m (Case e list) =
  Case (permute m e) (map (\ (pat,val) -> (pat, permute (reducedMap pat) val)) list)
    where
      reducedMap pat = (S.foldr M.delete m (patternVars pat))

freeMVars :: [Scheme] -> [MVar]
freeMVars tys = foldr (\ (Forall _ body) -> go body) [] tys
  where
    go (MVar tv) acc
      | tv `elem` acc      = acc
      | otherwise          = tv:acc
    go (TVar _) acc        = acc
    go (TConst _) acc      = acc
    go (Arrow arg res) acc = go arg (go res acc)
    go (App t1 t2) acc = go t1 (go t2 acc)

freeTVars :: [Scheme] -> [TVar]
freeTVars schemes = foldr (\ (Forall bound t) -> go bound t) [] schemes
  where
    go bound (TVar v) acc
      | v `elem` bound            = acc
      | v `elem` acc              = acc
      | otherwise                 = v:acc
    go _     (TConst _) acc       = acc
    go _     (MVar _) acc         = acc
    go bound (App t1 t2) acc      = go bound t1 (go bound t2 acc)
    go bound (Arrow arg res) acc  = go bound arg (go bound res acc)

type TyEnv = M.Map TVar Type
substitute :: TyEnv -> Type -> Type
substitute env (Arrow arg res) = Arrow (substitute env arg) (substitute env res)
substitute env (App t1 t2) = App (substitute env t1) (substitute env t2)
substitute env (TVar v) =
  case M.lookup v env of
    Just x -> x
    _ -> TVar v
substitute _ t = t

tyVarId :: TVar -> Id
tyVarId (Skolem i _) = i
tyVarId (Bound i) = i

instance Show MVar where
  show (Meta u _) = "?" ++ show u

instance Show TVar where
  show (Bound s) = s
  show (Skolem s u) = s ++ "#" ++ show u

instance Show Type where
  show (Arrow arg@(Arrow _ _) res) = "(" ++ show arg ++ ") -> " ++ show res
  show (Arrow arg res) = show arg ++ " -> " ++ show res
  show (TVar tvar) = show tvar
  show (MVar mvar) = show mvar
  show (TConst name) = name

  show (App fn arg) = lhs fn ++ " " ++ rhs arg
    where
      lhs t@(Arrow _ _) = "(" ++ show t ++ ")"
      lhs t = show t

      rhs t@(Arrow _ _) = "(" ++ show t ++ ")"
      rhs t@(App _ _) = "(" ++ show t ++ ")"
      rhs t = show t

instance Show Scheme where
  show (Forall [] body) = show body
  show (Forall args body) = "forall " ++ intercalate " " (map show args) ++ ". " ++ show body

class Displayable a where
  display :: a -> Doc

-- | Replace characters "@\=+-!:<>.^#$*%/|&~?" by fixed strings
generateCName :: String -> String
generateCName name =
  "fn_" ++ concat (map go name)
    where
      go '@' = "__a__"
      go '\\' = "__b__"
      go '=' = "__eq__"
      go '+' = "__plus__"
      go '-' = "__minus__"
      go '!' = "__not__"
      go ':' = "__dc__"
      go '<' = "__lt__"
      go '>' = "__gt__"
      go '.' = "__dot__"
      go '^' = "__xor__"
      go '#' = "__dash__"
      go '$' = "__dolar__"
      go '*' = "__mul__"
      go '%' = "__mod__"
      go '/' = "__div__"
      go '|' = "__bar__"
      go '&' = "__and__"
      go '~' = "__tilde__"
      go '?' = "__question__"
      go c = [c]
