import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO ()
import System.Environment (getArgs)

-- Data types for expressions and types
data Type = TInt
          | TBool
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
          | Abs VarId Type Expr
          | App Expr Expr
          | LetIn VarId Type Expr Expr
          deriving (Eq, Ord, Read, Show)

-- Typing environment
type Env = Map VarId Type

-- Auxiliary function to check types for arithmetic expressions
typingArith :: Maybe Type -> Maybe Type -> Maybe Type
typingArith (Just TInt) (Just TInt) = Just TInt
typingArith _ _ = Nothing

-- Auxiliary function to check types for equality expressions
typingEq :: Maybe Type -> Maybe Type -> Maybe Type
typingEq (Just TInt) (Just TInt) = Just TBool
typingEq (Just TBool) (Just TBool) = Just TBool
typingEq _ _ = Nothing

-- Main type checking function
typing :: Env -> Expr -> Maybe Type
typing _ (CInt _) = Just TInt
typing _ (CBool _) = Just TBool
typing env (Var x) = Map.lookup x env
typing env (Plus e1 e2) = typingArith (typing env e1) (typing env e2)
typing env (Minus e1 e2) = typingArith (typing env e1) (typing env e2)
typing env (Equal e1 e2) = typingEq (typing env e1) (typing env e2)
typing env (ITE e1 e2 e3) = do
    t1 <- typing env e1
    if t1 == TBool then do
        t2 <- typing env e2
        t3 <- typing env e3
        if t2 == t3 then Just t2 else Nothing
    else
        Nothing
typing env (Abs x t e) = do
    t' <- typing (Map.insert x t env) e
    return $ TArr t t'
typing env (App e1 e2) = do
    t1 <- typing env e1
    case t1 of
        TArr tArg tRes -> do
            t2 <- typing env e2
            if tArg == t2 then Just tRes else Nothing
        _ -> Nothing
typing env (LetIn x t e1 e2) = do
    t1 <- typing env e1
    typing (Map.insert x t env) e2

-- Function to read expressions from string
readExpr :: String -> Expr
readExpr = read

-- Function to handle type checking and produce output string
typeCheck :: Expr -> String
typeCheck expr =
    case typing Map.empty expr of
        Just t -> show t
        Nothing -> "Type Error"

-- Main function to handle IO
main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let exprs = lines contents
    let results = map (typeCheck . readExpr) exprs
    mapM_ putStrLn results
