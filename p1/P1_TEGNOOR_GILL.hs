import qualified Data.Map.Strict as Map
import System.Environment (getArgs)


type VarAsgn = Map.Map VarId Bool
type VarId = String

data Prop = Const Bool
            | Var VarId
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop
            | Iff Prop Prop 
            deriving (Eq, Read, Show)

findVarIds :: Prop -> [VarId]
findVarIds (Const _) = []
findVarIds (Var p) = [p]
findVarIds (Not p) = findVarIds p
findVarIds (And p1 p2) = findVarIds p1 ++ findVarIds p2
findVarIds (Or p1 p2) = findVarIds p1 ++ findVarIds p2
findVarIds (Imply p1 p2) = findVarIds p1 ++ findVarIds p2
findVarIds (Iff p1 p2) = findVarIds p1 ++ findVarIds p2

genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns [] = [Map.empty]
genVarAsgns (x:xs) = [Map.insert x b a | b <- [True, False], a <- genVarAsgns xs]

eval :: Prop -> VarAsgn -> Bool
eval (Const b) a = b
eval (Var p) a = Map.findWithDefault False p a
eval (Not p) a = not (eval p a)
eval (And p1 p2) a = eval p1 a && eval p2 a
eval (Or p1 p2) a = eval p1 a || eval p2 a 
eval (Imply p1 p2) a = not (eval p1 a) || eval p2 a 
eval (Iff p1 p2) a = eval p1 a == eval p2 a 

sat :: Prop -> Bool
sat p = any (eval p) (genVarAsgns (findVarIds  p))

readFormula :: String -> Prop
readFormula = read

checkFormula :: String -> String
checkFormula s 
    | sat(readFormula s) = "SAT"
    | otherwise = "UNSAT"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            contents <- readFile fileName
            let formulaStrings = lines contents
            mapM_ (putStrLn . checkFormula) formulaStrings
