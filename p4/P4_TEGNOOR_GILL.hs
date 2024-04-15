import System.IO ()
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Lazy


type VarId = String

data Expr = CInt Int
          | CBool Bool
          | Var VarId
          | Plus Expr Expr
          | Minus Expr Expr
          | Equal Expr Expr
          | ITE Expr Expr Expr
          | Abs VarId Expr
          | App Expr Expr
          | LetIn VarId Expr Expr
          deriving (Eq, Ord, Read, Show)

data Type = TInt
          | TBool
          | TError
          | TVar Int
          | TArr Type Type
          deriving (Eq, Ord, Read, Show)

data Constraint = CEq Type Type
                | CError
                deriving (Eq, Ord, Read, Show)

type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]

type Env = Map.Map VarId Type

type InferState a = State Int a


getFreshTVar :: InferState Type
getFreshTVar = do
    n <- get
    put (n + 1)
    return (TVar n)


infer :: Env -> Expr -> InferState (Type, ConstraintSet)
infer g (CInt _) = return (TInt, Set.empty)
infer g (CBool _) = return (TBool, Set.empty)
infer g (Var x) = case Map.lookup x g of
    Just t -> return (t, Set.empty)
    Nothing -> error "Variable not found in environment"
infer g (Plus e1 e2) = do
    (t1, c1) <- infer g e1
    (t2, c2) <- infer g e2
    let c = Set.union c1 (Set.union c2 (Set.fromList [CEq t1 TInt, CEq t2 TInt]))
    return (TInt, c)
infer g (Minus e1 e2) = do
    (t1, c1) <- infer g e1
    (t2, c2) <- infer g e2
    let c = Set.union c1 (Set.union c2 (Set.fromList [CEq t1 TInt, CEq t2 TInt]))
    return (TInt, c)
infer g (Equal e1 e2) = do
    (t1, c1) <- infer g e1
    (t2, c2) <- infer g e2
    let c = Set.union c1 (Set.union c2 (Set.fromList [CEq t1 t2]))
    return (TBool, c)
infer g (ITE e1 e2 e3) = do
    (t1, c1) <- infer g e1
    (t2, c2) <- infer g e2
    (t3, c3) <- infer g e3
    let c = Set.union c1 (Set.union c2 (Set.union c3 (Set.fromList [CEq t1 TBool, CEq t2 t3])))
    return (t2, c)
infer g (Abs x e) = do
    y <- getFreshTVar
    (t, c) <- infer (Map.insert x y g) e
    return (TArr y t, c)
infer g (App e1 e2) = do
    (t1, c1) <- infer g e1
    (t2, c2) <- infer g e2
    y <- getFreshTVar
    let c = Set.union c1 (Set.union c2 (Set.fromList [CEq t1 (TArr t2 y)]))
    return (y, c)
infer g (LetIn x e1 e2) = do
    (t1, c1) <- infer g e1
    (t2, c2) <- infer (Map.insert x t1 g) e2
    return (t2, Set.union c1 c2)




inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr expr = evalState (infer Map.empty expr) 0


toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toList


type Substitution = Map.Map Type Type

applySub :: Substitution -> Type -> Type
applySub sub t@(TVar x) = Map.findWithDefault t t sub
applySub sub (TArr t1 t2) = TArr (applySub sub t1) (applySub sub t2)
applySub _ t = t


applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList sub = map applyConstraint
  where
    applyConstraint :: Constraint -> Constraint
    applyConstraint (CEq t1 t2) = CEq (applySub sub t1) (applySub sub t2)
    applyConstraint c = c


composeSub :: Substitution -> Substitution -> Substitution
composeSub sub1 sub2 = Map.map (applySub sub1) sub2 `Map.union` sub1


tvars :: Type -> Set.Set Type
tvars (TVar x) = Set.singleton (TVar x)
tvars (TArr t1 t2) = Set.union (tvars t1) (tvars t2)
tvars _ = Set.empty


unify :: ConstraintList -> Maybe Substitution
unify [] = Just Map.empty
unify (CEq t1 t2 : cs)
    | t1 == t2 = unify cs
    | TVar _ <- t1, not (t1 `Set.member` tvars t2) = do
        sub <- unify (applySubToCstrList (Map.singleton t1 t2) cs)
        return (Map.insert t1 t2 sub)
    | TVar _ <- t2, not (t2 `Set.member` tvars t1) = do
        sub <- unify (applySubToCstrList (Map.singleton t2 t1) cs)
        return (Map.insert t2 t1 sub)
unify _ = Nothing


typeInfer :: Expr -> String
typeInfer expr =
    let ty = typing expr
    in case ty of
        Just t -> show (relabel t)
        Nothing -> "Type Error"

typing :: Expr -> Maybe Type
typing expr =
    let (t, c) = inferExpr expr
        sub = unify (toCstrList c)
    in case sub of
        Just s -> Just (applySub s t)
        Nothing -> Nothing


type RelabelState a = State (Map.Map Int Int) a
relabel :: Type -> Type
relabel t = evalState (go t) Map.empty
  where
    go :: Type -> RelabelState Type
    go TInt = return TInt
    go TBool = return TBool
    go TError = return TError
    go (TVar x) = do
      m <- get
      case Map.lookup x m of
        Just v -> return (TVar v)
        Nothing -> do
          let n = 1 + Map.size m
          put (Map.insert x n m)
          return (TVar n)
    go (TArr t1 t2) = do
      t1' <- go t1
      t2' <- go t2
      return (TArr t1' t2')



main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            contents <- readFile fileName
            let exprStrings = lines contents
                expressions = map readExpr exprStrings
            -- Print out the environment for each expression
            let envs = map (\expr -> inferExpr expr >>= \(t, _) -> return (expr, t)) expressions
            mapM_ (\(expr, t) -> putStrLn $ "Expression: " ++ show expr ++ ", Type: " ++ show t) envs
            -- Perform type inference and print the results
            let results = map typeInfer expressions
            mapM_ putStrLn results


readExpr :: String -> Expr
readExpr = read
