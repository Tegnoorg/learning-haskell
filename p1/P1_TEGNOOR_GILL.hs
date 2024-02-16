import qualified Data.Map.Strict as Map


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
findVarIds (Var v) = [v]
findVarIds (Not p) = findVarIds p
findVarIds (And p p1) = findVarIds p ++ findVarIds p1
findVarIds (Or p p1) = findVarIds p ++ findVarIds p1
findVarIds (Imply p p1) = findVarIds p ++ findVarIds p1
findVarIds (Iff p p1) = findVarIds p ++ findVarIds p1

genVarAsgns :: [VarId] -> [VarAsgn] 
genVarAsgns vars = map (Map.fromList . zip vars) (sequence (replicate (length vars) [True, False]))

eval :: Prop -> VarAsgn -> Bool
eval (Const p) _ = p
eval (Var p) a = case Map.lookup p a of
    Just p -> p
    Nothing -> error "error"
eval (Not p) a = not (eval p a)
eval (And p1 p2) a = eval p1 a && eval p2 a
eval (Or p1 p2) a = eval p1 a || eval p2 a
eval (Imply p1 p2) a = not (eval p1 a) || eval p2 a
eval (Iff p1 p2) a = eval p1 a == eval p2 a

sat :: Prop -> Bool 
sat p = any (eval p) (genVarAsgns (findVarIds p))

readFormula :: String -> Prop
readFormula  = read

checkFormula :: String -> String
checkFormula formulaStr =
    if sat (readFormula formulaStr)
        then "SAT"
        else "UNSAT"

main :: IO ()
main = do
    contents <- readFile "formulas.txt"
    let formulaStrings = lines contents
    mapM_ (putStrLn . checkFormula) formulaStrings


