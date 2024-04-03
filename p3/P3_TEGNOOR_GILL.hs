import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO ()
import System.Environment (getArgs)


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


type Env = Map VarId Type


typingArith :: Maybe Type -> Maybe Type -> Maybe Type
typingArith (Just TInt) (Just TInt) = Just TInt
typingArith _ _ = Nothing

typingEq :: Maybe Type -> Maybe Type -> Maybe Type 
typingEq (Just TInt) (Just TInt) = Just TBool
typingEq (Just TBool) (Just TBool) = Just TBool
typingEq _ _ = Nothing

typing :: Env -> Expr -> Maybe Type
typing _ (CInt x) = Just TInt
typing _ (CBool x) = Just TBool
typing env (Var x) = Map.lookup x env
typing env (Plus x y) = typingArith (typing env x) (typing env y)
typing env (Minus x y) = typingArith (typing env x) (typing env y)
typing env (Equal x y) = typingEq (typing env x) (typing env y)
typing env (ITE x y z) = do
    x1 <- typing env x
    y1 <- typing env y
    z1 <- typing env z
    if x1 == TBool && y1 == z1 
        then Just z1 
        else Nothing
typing env (Abs x y z) = 
    case typing (Map.insert x y env) z of
        Just y1 -> Just (TArr y y1)
        _ -> Nothing
typing env (App x y ) = do
    x1 <- typing env x
    case x1 of
        TArr z u -> do
            y1 <- typing env y
            if z == y1 then Just u else Nothing
        _ -> Nothing
typing env (LetIn x t e1 e2) = do
    t1 <- typing env e1
    if t1 == t
        then typing (Map.insert x t env) e2
        else Nothing

readExpr :: String -> Expr 
readExpr = read

typeCheck :: Expr -> String
typeCheck x = maybe "Type Error" show (typing Map.empty x)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            contents <- readFile fileName
            let formulaStrings = lines contents
            mapM_ (putStrLn . typeCheck . readExpr) formulaStrings
