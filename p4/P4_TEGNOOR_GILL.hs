import System.Environment (getArgs)
import Control.Monad.State.Lazy
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Type = TInt 
    | TBool 
    | TError 
    | TVar Int 
    | TArr Type Type
  deriving (Eq, Ord, Read, Show)

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

type Env = Map VarId Type
type InferState a = State Int a
type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]
type Substitution = Map.Map Type Type
data Constraint = CEq Type Type | CError
  deriving (Eq, Ord, Read, Show)
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
  Nothing -> return (TError, Set.empty)
infer g (Plus e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c = Set.unions [c1, c2, Set.fromList [CEq t1 TInt, CEq t2 TInt]]
  return (TInt, c)
infer g (Minus e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c = Set.unions [c1, c2, Set.fromList [CEq t1 TInt, CEq t2 TInt]]
  return (TInt, c)
infer g (Equal e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c = Set.unions [c1, c2, Set.fromList [CEq t1 t2]]
  return (TBool, c)
infer g (ITE e1 e2 e3) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  (t3, c3) <- infer g e3
  let c = Set.unions [c1, c2, c3, Set.fromList [CEq t1 TBool, CEq t2 t3]]
  return (t2, c)
infer g (Abs x e) = do
  v <- getFreshTVar
  (t, c) <- infer (Map.insert x v g) e
  return (TArr v t, c)
infer g (App e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  v <- getFreshTVar
  let c = Set.unions [c1, c2, Set.fromList [CEq t1 (TArr t2 v)]]
  return (v, c)
infer g (LetIn x e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer (Map.insert x t1 g) e2
  return (t2, Set.union c1 c2)

inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr e = evalState (infer Map.empty e) 0

toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toList

applySub :: Substitution -> Type -> Type
applySub _ TInt = TInt
applySub _ TBool = TBool
applySub _ TError = TError
applySub s (TVar x) = case Map.lookup (TVar x) s of
  Just t -> t
  Nothing -> TVar x
applySub s (TArr t1 t2) = TArr (applySub s t1) (applySub s t2)

applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList s = map (\(CEq t1 t2) -> CEq (applySub s t1) (applySub s t2))

composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2 = Map.union (Map.map (applySub s1) s2) s1

tvars :: Type -> Set.Set Type
tvars TInt = Set.empty
tvars TBool = Set.empty
tvars TError = Set.empty
tvars (TVar x) = Set.singleton (TVar x)
tvars (TArr t1 t2) = Set.union (tvars t1) (tvars t2)

unify :: ConstraintList -> Maybe Substitution
unify [] = Just Map.empty
unify (CEq t1 t2 : cs) =
  case (t1, t2) of
    (TInt, TInt) -> unify cs
    (TBool, TBool) -> unify cs
    (TVar x, _) | t2 `elem` tvars (TVar x) -> Nothing
                | otherwise -> fmap (\s -> composeSub s (Map.singleton (TVar x) t2)) (unify (applySubToCstrList (Map.singleton (TVar x) t2) cs))
    (_, TVar x) | t1 `elem` tvars (TVar x) -> Nothing
                | otherwise -> fmap (\s -> composeSub s (Map.singleton (TVar x) t1)) (unify (applySubToCstrList (Map.singleton (TVar x) t1) cs))
    (TArr a b, TArr c d) -> unify (CEq a c : CEq b d : cs)
    _ -> Nothing


typing :: Expr -> Maybe Type
typing e = case evalState (infer Map.empty e) 0 of
  (t, c) -> case unify (toCstrList c) of
    Just s -> Just (applySub s t)
    Nothing -> Nothing

typeInfer :: Expr -> String
typeInfer e = case typing e of
  Just t -> show (relabel t)
  Nothing -> "Type Error"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      let formulaStrings = lines contents
      mapM_ (putStrLn . typeInfer . read) formulaStrings
