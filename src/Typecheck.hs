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
    , var_env :: M.Map Id Sigma }

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

runTC :: [(Id, Sigma)] -> TC a -> IO (Either String a)
runTC bindings TC{unTC=tc} = do
  ref <- newIORef 0
  let env = TCEnv { unique= ref, var_env= M.fromList bindings }
  tc env

lift :: IO a -> TC a
lift io = TC{unTC= \ _ -> Right <$> io}

---------------------------------------------------------------------------------------------------
  -- Read/upadte environment state
---------------------------------------------------------------------------------------------------

envScope :: Id -> Sigma -> TC a -> TC a
envScope var ty TC{unTC=tc} = TC{unTC= \ env -> tc env{var_env= M.insert var ty (var_env env)}}

getEnv :: TC (M.Map Id Sigma)
getEnv = TC {unTC= \env -> pure (Right (var_env env))}

lookupVar :: Id -> TC Sigma
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

readMVar :: MVar -> TC (Maybe Tau)
readMVar (Meta _ ref) = lift (readIORef ref)

writeMVar :: MVar -> Tau -> TC ()
writeMVar (Meta _ ref) ty = do
  lift (writeIORef ref (Just ty))

---------------------------------------------------------------------------------------------------
  -- Read free variables of a list of expressions after zonking (mvars, tbars...)
---------------------------------------------------------------------------------------------------

getEnvTypes :: TC [Type]
getEnvTypes = do
  env <- getEnv
  return $ M.elems env

getFreeMVars :: [Type] -> TC [MVar]
getFreeMVars types = do
  types' <- mapM zonkType types
  return $ freeMVars types'

getFreeTVars :: [Type] -> TC [TVar]
getFreeTVars types = do
  types' <- mapM zonkType types
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
  return (SkolemTv (tyVarId ty) uniq)

---------------------------------------------------------------------------------------------------
  -- Forall/Exists manipulation: instantiation of foralls, skolemisation and quantification
---------------------------------------------------------------------------------------------------

-- | Instantiate a @Forall@ type using fresh meta variables
instantiate :: Sigma -> TC Rho
instantiate (Forall tvs ty) = do
  tvs' <- mapM (\ _ -> MVar <$> newMVar) tvs
  return $ substitute (M.fromList (zip tvs tvs')) ty
instantiate ty = return ty

-- | Deeply skolemise a type (replace it's foralls by fresh skolem variables), this function return
-- the generated skolem variables, and the skolemised type
skolemise :: Sigma -> TC ([TVar], Rho)
skolemise (Forall tvars body) = do
  sk1 <- mapM newSkolem tvars
  (sk2, new_body) <- skolemise (substitute (M.fromList (zip tvars (map TVar sk1))) body)
  return (sk1++sk2, new_body)
skolemise (Arrow arg res) = do
  (sk, res') <- skolemise res
  return (sk, Arrow arg res')
skolemise ty =
  return ([], ty)

-- | Quantify (using a @Forall@) over a set of meta variables
quantify :: [MVar] -> Rho -> TC Sigma
quantify tvs ty = do
  mapM_ bind (zip tvs new_binders)
  ty' <- zonkType ty
  return (Forall new_binders ty')

  where
    used_binders = bindersTVar ty
    new_binders = take (length tvs) (all_binders Data.List.\\ used_binders)
    bind (tv, ident) = writeMVar tv (TVar ident)

    all_binders =
      [BoundTv [x] | x <- ['a'..'z']] ++
      [BoundTv (x:show i) | i <- [1::Int ..], x <- ['a'..'z']]


---------------------------------------------------------------------------------------------------
  -- Zonkify types: replace all their known meta variables by their value
---------------------------------------------------------------------------------------------------

-- | Replace all the known meta variables by their value
zonkType :: Type -> TC Type
zonkType (Forall ns ty) = do
  ty' <- zonkType ty
  return (Forall ns ty')
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
unify :: Tau -> Tau -> TC ()
unify (Arrow a1 r1) (Arrow a2 r2)          = unify a1 a2 >> unify r1 r2
unify (TVar v1)     (TVar v2) | v1 == v2   = pure ()
unify (MVar v1)     (MVar v2) | v1 == v2   = pure ()
unify (MVar v1)     ty                     = unifyMVar v1 ty
unify ty            (MVar v2)              = unifyMVar v2 ty
unify TInt          TInt                   = pure ()
unify t1            t2                     =
  fail ("can't unify: `" ++ show t1 ++ "` and `" ++ show t2 ++ "`")

-- | Unity a meta variable with a tau type (without quantification)
unifyMVar :: MVar -> Tau -> TC ()
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
          let mvars = freeMVars [ty2']
          if v1 `elem` mvars
          then fail ""
          else writeMVar v1 ty2

-- | Unify as an arrow: "force" a type to be a function
unifyArrow :: Rho -> TC (Sigma, Rho)
unifyArrow (Arrow arg res) = return (arg, res)
unifyArrow tau = do
  arg <- MVar <$> newMVar
  res <- MVar <$> newMVar
  unify tau (arg --> res)
  return (arg, res)


data Expected a = Infer (IORef a) | Check a

-- | Check that an expression can be typed using a given rho expression
checkRho :: Exp -> Rho -> TC ()
checkRho expr ty = typecheckRho expr (Check ty)


-- | Infer the type of an expression as a Rho type
inferRho :: Exp -> TC Rho
inferRho expr = do
  ref <- lift $ newIORef (error "inferRho: empty result")
  typecheckRho expr (Infer ref)
  lift $ readIORef ref

-- | Typecheck an expression in the domain of Rho types
typecheckRho :: Exp -> Expected Rho -> TC ()
typecheckRho (Lit Undefined) expected = do
  ty <- MVar <$> newMVar
  instSigma ty expected
typecheckRho (Lit _) expected = instSigma TInt expected
typecheckRho (Var v) expected = do
  sigma <- lookupVar v
  instSigma sigma expected
typecheckRho (Apply fun (arg:args@(_:_))) expected = do
  typecheckRho (Apply (Apply fun [arg]) args) expected
typecheckRho (Apply fun []) expected =
  typecheckRho fun expected
typecheckRho (Apply fun [arg]) expected = do
  fun_type <- inferRho fun
  (arg_type, res_type) <- unifyArrow fun_type
  checkSigma arg arg_type
  instSigma res_type expected
typecheckRho (Lambda (var:vars@(_:_)) body) expected =
  typecheckRho (Lambda [var] (Lambda vars body)) expected
typecheckRho (Lambda [] body) expected =
  typecheckRho body expected
typecheckRho (Lambda [var] body) (Check fun_ty) = do
  (arg_type, res_type) <- unifyArrow fun_ty
  envScope var arg_type (checkRho body res_type)
typecheckRho (Lambda [var] body) (Infer ref) = do
  arg_type <- MVar <$> newMVar
  res_type <- envScope var arg_type (inferRho body)
  lift $ writeIORef ref (arg_type --> res_type)
typecheckRho (Annot body annot) expected = do
  checkSigma body annot
  instSigma annot expected
typecheckRho (Switch cond list) expected = do
  checkSigma cond TInt
  out <- MVar <$> newMVar
  forM_ list \ (_,val) -> do
    val_type <- inferRho val
    unify val_type out
  instSigma out expected
typecheckRho (LetIn var e1 e2) expected = do
  meta <- MVar <$> newMVar
  var_type <- envScope var meta (inferSigma e1)
  envScope var var_type (typecheckRho e2 expected)

-- | Infer the type of an expression as a sigma type: quantify over all the meta-variables of the
-- expression (except the mvars from the environment)
inferSigma :: Exp -> TC Sigma
inferSigma expr = do
  exp_ty <- inferRho expr
  env_tvars <- getEnvTypes
  env_mvars <- getFreeMVars env_tvars
  exp_mvars <- getFreeMVars [exp_ty]
  let forall_mvars = exp_mvars Data.List.\\ env_mvars
  quantify forall_mvars exp_ty

-- | Check the type of an expression over a sigma expression
checkSigma :: Exp -> Sigma -> TC ()
checkSigma expr sigma = do
  (skol, rho) <- skolemise sigma
  checkRho expr rho
  env_types <- getEnvTypes
  free <- getFreeTVars (sigma : env_types)
  let bad = filter (`elem` free) skol
  check (null bad) ("Type not polymorphic enough")

---------------------------------------------------------------------------------------------------
  -- Check that a types subsume another
---------------------------------------------------------------------------------------------------

-- | Check that one sigma type subsume another
subsumptionCheck :: Sigma -> Sigma -> TC ()
subsumptionCheck sigma1 sigma2 = do
  (skl2, rho2) <- skolemise sigma2
  subsumptionCheckRho sigma1 rho2
  tvars <- getFreeTVars [sigma1,sigma2]
  let bad = filter (`elem` tvars) skl2
  check (null bad) ("subsumption failed: `" ++ show sigma1 ++ "` and `" ++ show sigma2 ++ "`")

-- | Check that a sigma expression subsume a rho type
subsumptionCheckRho :: Sigma -> Rho -> TC ()
subsumptionCheckRho sigma@(Forall _ _) rho2 = do
  rho1 <- instantiate sigma
  subsumptionCheckRho rho1 rho2

subsumptionCheckRho rho1 (Arrow arg2 res2) = do
  (arg1, res1) <- unifyArrow rho1
  subsumptionCheck arg2 arg1
  subsumptionCheckRho res1 res2

subsumptionCheckRho (Arrow arg1 res1) rho2 = do
  (arg2, res2) <- unifyArrow rho2
  subsumptionCheck arg2 arg1
  subsumptionCheckRho res1 res2

subsumptionCheckRho tau1 tau2 = unify tau1 tau2

-- | Instantiate a sigma type
instSigma :: Sigma -> Expected Rho -> TC ()
instSigma t1 (Check t2) = subsumptionCheckRho t1 t2
instSigma t1 (Infer r) = do
  t1' <- instantiate t1
  lift $ writeIORef r t1'

test :: IO ()
test = wrap $ runTC [("+",TInt --> (TInt --> TInt))] do
  m0 <- MVar <$> newMVar
  envScope "-" m0 $ do
    debug $ Lambda ["x"] (lit 42 -: (Var "x" +: lit 43))
  debug rec_letin
  subsumptionCheck lhs rhs
  where
    lit i = Lit (Int i)
    (+:) a b = Apply (Var "+") [a, b]
    (-:) a b = Apply (Var "-") [a, b]

    debug expression = inferRho expression >>= zonkType >>= lift . print

    x = BoundTv "x"
    lhs = Forall [x] (Arrow (TVar x) (TVar x))

    rhs = Arrow TInt TInt

    rec_letin =
      LetIn "f" (Lambda ["x"] (Apply (Var "+") [Var "x", Apply (Var "f") [Var "x"]])) (Var "f")

    wrap io = do
      res <- io
      case res of
        Right _ -> print "success"
        Left msg -> print ("failure: " ++ msg)
