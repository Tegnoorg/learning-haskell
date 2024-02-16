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
genVarAsgns = error "Not done" 

eval :: Prop -> VarAsgn -> Bool
eval = error "Not done"

readFormula :: String -> Prop
readFormula = error "Not done"

checkFormula :: String -> String
checkFormula = error "Not done"