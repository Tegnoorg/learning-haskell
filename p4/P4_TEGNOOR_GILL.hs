import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO ()
import System.Environment (getArgs)
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