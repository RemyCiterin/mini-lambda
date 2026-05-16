module Typecheck where

import AST
import qualified Data.List
import qualified Data.Map as M
import Control.Monad
import Data.IORef

---------------------------------------------------------------------------------------------------
  -- Environment and type checking monad
---------------------------------------------------------------------------------------------------

data TCEnv =
  TCEnv
    { unique :: IORef Uniq
    , var_env :: M.Map Id Scheme }

data TC a = TC { unTC :: TCEnv -> IO (Either String a) }

instance Monad TC where
  m >>= f =
    TC
      { unTC =
        \ env -> do
          res <- unTC m env
          case res of
            Left err -> pure (Left err)
            Right a -> unTC (f a) env
      }
  return = pure

instance MonadFail TC where
  fail err = TC { unTC = \ _ -> pure (Left err) }

instance Functor TC where
  fmap = liftM

instance Applicative TC where
  pure a = TC { unTC = \ _ -> pure (Right a) }
  (<*>) = ap

check :: Bool -> String -> TC ()
check True _ = pure ()
check False s = fail s

runTC :: [(Id, Scheme)] -> TC a -> IO (Either String a)
runTC bindings TC{unTC=tc} = do
  ref <- newIORef 0
  let env = TCEnv { unique= ref, var_env= M.fromList bindings }
  tc env

lift :: IO a -> TC a
lift io = TC{unTC= \ _ -> Right <$> io}

---------------------------------------------------------------------------------------------------
  -- Read/upadte environment state
---------------------------------------------------------------------------------------------------

envScope :: Id -> Scheme -> TC a -> TC a
envScope var ty TC{unTC=tc} = TC{unTC= \ env -> tc env{var_env= M.insert var ty (var_env env)}}

getEnv :: TC (M.Map Id Scheme)
getEnv = TC {unTC= \env -> pure (Right (var_env env))}

lookupVar :: Id -> TC Scheme
lookupVar var = do
  env <- getEnv
  case M.lookup var env of
    Just x -> return x
    _ -> fail ("Not in scope: " ++ show var)

newUnique :: TC Uniq
newUnique =
  TC { unTC = \ TCEnv{unique= ref} ->
    do
      uniq <- readIORef ref
      writeIORef ref (uniq + 1)
      pure (Right uniq) }

---------------------------------------------------------------------------------------------------
  -- Read/write meta variable states
---------------------------------------------------------------------------------------------------

readMVar :: MVar -> TC (Maybe Type)
readMVar (Meta _ ref) = lift (readIORef ref)

writeMVar :: MVar -> Type -> TC ()
writeMVar (Meta _ ref) ty = do
  lift (writeIORef ref (Just ty))

---------------------------------------------------------------------------------------------------
  -- Read free variables of a list of expressions after zonking (mvars, tbars...)
---------------------------------------------------------------------------------------------------

getEnvSchemes :: TC [Scheme]
getEnvSchemes = do
  env <- getEnv
  return $ M.elems env

getFreeMVars :: [Scheme] -> TC [MVar]
getFreeMVars types = do
  types' <- mapM zonkScheme types
  return $ freeMVars types'

getFreeTVars :: [Scheme] -> TC [TVar]
getFreeTVars types = do
  types' <- mapM zonkScheme types
  return $ freeTVars types'

---------------------------------------------------------------------------------------------------
  -- Generate fresh variables (meta and skolemised variables)
---------------------------------------------------------------------------------------------------

-- | Generate a fresh meta variable
newMVar :: TC MVar
newMVar = do
  uniq <- newUnique
  ref <- lift (newIORef Nothing)
  return (Meta uniq ref)

-- | Generate a fresh skolem variable reusing the name of another variable to pretty printing
newSkolem :: TVar -> TC TVar
newSkolem ty = do
  uniq <- newUnique
  return (Skolem (tyVarId ty) uniq)

---------------------------------------------------------------------------------------------------
  -- Forall/Exists manipulation: instantiation of foralls, skolemisation and quantification
---------------------------------------------------------------------------------------------------

-- | Instantiate a @Forall@ type using fresh meta variables
instantiate :: Scheme -> TC Type
instantiate (Forall tvs ty) = do
  tvs' <- mapM (\ _ -> MVar <$> newMVar) tvs
  return $ substitute (M.fromList (zip tvs tvs')) ty

-- | Deeply skolemise a type (replace it's foralls by fresh skolem variables), this function return
-- the generated skolem variables, and the skolemised type
skolemise :: Scheme -> TC ([TVar], Type)
skolemise (Forall tvars body) = do
  sk <- mapM newSkolem tvars
  let new_body = substitute (M.fromList (zip tvars (map TVar sk))) body
  return (sk, new_body)

-- | Quantify (using a @Forall@) over a set of meta variables
quantify :: [MVar] -> Type -> TC Scheme
quantify tvs ty = do
  mapM_ bind (zip tvs new_binders)
  ty' <- zonkType ty
  return (Forall new_binders ty')

  where
    used_binders = freeTVars [Forall [] ty]
    new_binders = take (length tvs) (all_binders Data.List.\\ used_binders)
    bind (tv, ident) = writeMVar tv (TVar ident)

    all_binders =
      [Bound [x] | x <- ['a'..'z']] ++
      [Bound (x:show i) | i <- [1::Int ..], x <- ['a'..'z']]

---------------------------------------------------------------------------------------------------
  -- Zonkify types: replace all their known meta variables by their value
---------------------------------------------------------------------------------------------------

-- | Replace all the known meta variables by their value
zonkScheme :: Scheme -> TC Scheme
zonkScheme (Forall ns ty) = do
  ty' <- zonkType ty
  return (Forall ns ty')

-- | Replace all the known meta variables by their value
zonkType :: Type -> TC Type
zonkType (App lhs rhs) = do
  lhs' <- zonkType lhs
  rhs' <- zonkType rhs
  return (App lhs' rhs')
zonkType (Arrow arg res) = do
  arg' <- zonkType arg
  res' <- zonkType res
  return (Arrow arg' res')
zonkType (MVar tv) = do
  x <- readMVar tv
  case x of
    Nothing -> return (MVar tv)
    Just ty -> do
      ty' <- zonkType ty
      writeMVar tv ty' -- "compress" the DAG to improve performance
      return ty'
zonkType t = pure t

---------------------------------------------------------------------------------------------------
  -- Unification: assert the equivalence of two types
---------------------------------------------------------------------------------------------------

-- | Unity two tau types (without quantification)
unify :: Type -> Type -> TC ()
unify (TVar (Bound _))   _                      = fail "PANIC: unpexpected bound variable"
unify _                  (TVar (Bound _))       = fail "PANIC: unpexpected bound variable"
unify (App l1 r1)        (App l2 r2)            = unify l1 l2 >> unify r1 r2
unify (Arrow a1 r1)      (Arrow a2 r2)          = unify a1 a2 >> unify r1 r2
unify (TVar v1)          (TVar v2) | v1 == v2   = pure ()
unify (MVar v1)          (MVar v2) | v1 == v2   = pure ()
unify (MVar v1)          ty                     = unifyMVar v1 ty
unify ty                 (MVar v2)              = unifyMVar v2 ty
unify (TConst s1)        (TConst s2) | s1 == s2 = pure ()
unify t1                 t2                     =
  fail ("can't prove the equality of `" ++ show t1 ++ "` and `" ++ show t2 ++ "`")

-- | Unity a meta variable with a tau type (without quantification)
unifyMVar :: MVar -> Type -> TC ()
unifyMVar v1 ty2 = do
  ty1 <- readMVar v1
  case ty1 of
    Just t -> unify t ty2
    Nothing -> do
      case ty2 of
        MVar v2 -> do
          ty2' <- readMVar v2
          case ty2' of
            Just t -> unify (MVar v1) t
            Nothing -> writeMVar v1 ty2
        _ -> do
          ty2' <- zonkType ty2
          let mvars = freeMVars [Forall [] ty2']
          if v1 `elem` mvars
          then fail ("can't prove equality between `"++show v1++"` and `"++show ty2'++"`")
          else writeMVar v1 ty2

-- | Unify as an arrow: "force" a type to be a function
unifyArrow :: Type -> TC (Type, Type)
unifyArrow (Arrow arg res) = return (arg, res)
unifyArrow tau = do
  arg <- MVar <$> newMVar
  res <- MVar <$> newMVar
  unify tau (arg --> res)
  return (arg, res)

data Expected a = Infer (IORef a) | Check a

-- | Check that an expression can be typed using a given type
checkType :: Exp -> Type -> TC ()
checkType expr ty = typecheckExp expr (Check ty)

-- | Infer the type of an expression as a Type type
inferType :: Exp -> TC Type
inferType expr = do
  ref <- lift $ newIORef (error "inferType: empty result")
  typecheckExp expr (Infer ref)
  lift $ readIORef ref

checkPattern :: Pattern -> Type -> TC () -> TC ()
checkPattern (PInt _) ty kont = unify ty (TConst "Int") >> kont
checkPattern (PVar v) ty kont = envScope v (Forall [] ty) kont
checkPattern Wildcard _  k = k
checkPattern (PCons cons args) ty kont = do
  scheme <- lookupVar cons
  arrow <- instantiate scheme
  go arrow args \ t -> unify ty t >> kont
  where
    go (Arrow arg res) (p:ps) k = checkPattern p arg (go res ps k)
    go _               (_:_)  _ = fail ("`"++cons++"` is applied to too many arguments")
    go res             []     k = k res

-- | typecheckExp an expression in the domain of types
typecheckExp :: Exp -> Expected Type -> TC ()
typecheckExp (Lit Undefined) expected =
  instScheme (Forall [Bound "a"] (TVar (Bound "a"))) expected
typecheckExp (Lit (CFun _ _)) expected =
  instScheme (Forall [Bound "a"] (TVar (Bound "a"))) expected
typecheckExp (Lit (Int _)) expected =
  instType (TConst "Int") expected
typecheckExp (Lit (Tag _)) expected =
  instType (TConst "Int") expected
typecheckExp (Var v) expected = do
  scheme <- lookupVar v
  instScheme scheme expected
typecheckExp (Lit (Cons c)) expected = do
  scheme <- lookupVar c
  instScheme scheme expected
typecheckExp (Apply fun (arg:args@(_:_))) expected = do
  typecheckExp (Apply (Apply fun [arg]) args) expected
typecheckExp (Apply fun []) expected =
  typecheckExp fun expected
typecheckExp (Apply fun [arg]) expected = do
  fun_type <- inferType fun
  (arg_type, res_type) <- unifyArrow fun_type
  checkType arg arg_type
  instType res_type expected
typecheckExp (Lambda (var:vars@(_:_)) body) expected =
  typecheckExp (Lambda [var] (Lambda vars body)) expected
typecheckExp (Lambda [] body) expected =
  typecheckExp body expected
typecheckExp (Lambda [var] body) (Check fun_ty) = do
  (arg_type, res_type) <- unifyArrow fun_ty
  envScope var (Forall [] arg_type) (checkType body res_type)
typecheckExp (Lambda [var] body) (Infer ref) = do
  arg_type <- MVar <$> newMVar
  res_type <- envScope var (Forall [] arg_type) (inferType body)
  lift $ writeIORef ref (arg_type --> res_type)
typecheckExp (Annot body annot) expected = do
  checkType body annot
  instType annot expected
-- typecheckExp (Switch cond list) expected = do
--   checkType cond (TConst "Int")
--   out <- MVar <$> newMVar
--   forM_ list \ (_,val) -> do
--     val_type <- inferType val
--     unify val_type out
--   instType out expected
typecheckExp (Case expr patterns) expected = do
  expr_ty <- inferType expr
  out <- MVar <$> newMVar
  forM_ patterns \ (pat,e) -> do
    checkPattern pat expr_ty (checkType e out)
  instType out expected
typecheckExp (LetIn var e1 e2) expected = do
  meta <- MVar <$> newMVar
  var_type <- envScope var (Forall [] meta) (inferScheme e1)
  envScope var var_type (typecheckExp e2 expected)

-- | Infer the type of an expression as a scheme: quantify over all the meta-variables of the
-- expression (except the mvars from the environment)
inferScheme :: Exp -> TC Scheme
inferScheme expr = do
  exp_ty <- inferType expr
  env_tvars <- getEnvSchemes
  env_mvars <- getFreeMVars env_tvars
  exp_mvars <- getFreeMVars [Forall [] exp_ty]
  let forall_mvars = exp_mvars Data.List.\\ env_mvars
  quantify forall_mvars exp_ty

-- | Check the type of an expression over a scheme
checkScheme :: Exp -> Scheme -> TC ()
checkScheme expr scheme = do
  (skol, ty) <- skolemise scheme
  checkType expr ty
  env_types <- getEnvSchemes
  free <- getFreeTVars (scheme : env_types)
  let bad = filter (`elem` free) skol
  check (null bad) ("Type not polymorphic enough")

---------------------------------------------------------------------------------------------------
  -- Check that a types subsume another
---------------------------------------------------------------------------------------------------

-- | Check that one scheme subsume another
subsumptionCheck :: Scheme -> Scheme -> TC ()
subsumptionCheck scheme1 scheme2 = do
  (skl2, ty2) <- skolemise scheme2
  subsumptionCheckType scheme1 ty2
  tvars <- getFreeTVars [scheme1,scheme2]
  let bad = filter (`elem` tvars) skl2
  check (null bad) ("subsumption failed: `" ++ show scheme1 ++ "` and `" ++ show scheme2 ++ "`")

-- | Check that a scheme subsume a type
subsumptionCheckType :: Scheme -> Type -> TC ()
subsumptionCheckType scheme@(Forall _ _) ty2 = do
  ty1 <- instantiate scheme
  unify ty1 ty2

-- | Instantiate a type
instType :: Type -> Expected Type -> TC ()
instType t1 (Check t2) = unify t1 t2
instType t1 (Infer r) = do
  lift $ writeIORef r t1

-- | Instantiate a scheme
instScheme :: Scheme -> Expected Type -> TC ()
instScheme t1 (Check t2) = subsumptionCheckType t1 t2
instScheme t1 (Infer r) = do
  t1' <- instantiate t1
  lift $ writeIORef r t1'

test :: IO ()
test = wrap $ runTC [("+",Forall [] $ int --> (int --> int))] do
  m0 <- MVar <$> newMVar
  envScope "-" (Forall [] m0) $ do
    debug $ Lambda ["x"] (lit 42 -: (Var "x" +: lit 43))
  debug rec_letin
  subsumptionCheck lhs rhs
  where
    lit i = Lit (Int i)
    (+:) a b = Apply (Var "+") [a, b]
    (-:) a b = Apply (Var "-") [a, b]

    debug expression = inferType expression >>= zonkType >>= lift . print

    x = Bound "x"
    y = Bound "y"
    int = TConst "Int"
    foo = TConst "foo"
    lhs = Forall [x,y] (TVar y --> TVar y)

    rhs = Forall [x] $ int --> (int --> int)

    rec_letin =
      LetIn "f" (Lambda ["x"] (Apply (Var "+") [Var "x", Apply (Var "f") [Var "x"]])) (Var "f")

    wrap io = do
      res <- io
      case res of
        Right _ -> print "success"
        Left msg -> print ("failure: " ++ msg)
