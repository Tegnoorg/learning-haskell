-- G1
-- Formula ::= Formula '<->' Formula | ImpTerm
-- ImpTerm ::= ImpTerm '->'  ImpTerm | AndTerm
-- AndTerm ::= AndTerm '\/'  AndTerm | OrTerm
-- OrTerm  ::= OrTerm  '/\'  OrTerm  | Factor
-- Factor  ::= '(' Formula ')' | 'T' | 'F' | Ident

-- G2
-- Formula ::= ImpTerm '<->' Formula | ImpTerm
-- ImpTerm ::= AndTerm '->'  ImpTerm | AndTerm
-- AndTerm ::= OrTerm  '\/'  AndTerm | OrTerm
-- OrTerm  ::= Factor  '/\'  OrTerm  | Factor
-- Factor  ::= '(' Formula ')' | 'T' | 'F' | Ident

import Data.Char (isSpace)
import Control.Applicative
import System.Environment (getArgs)
import Data.Functor

data Prop = Const Bool
        | Var String
        | Not Prop
        | And Prop Prop
        | Or Prop Prop
        | Imply Prop Prop
        | Iff Prop Prop
        deriving (Eq, Read, Show)

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\input -> case parse p input of
        [] -> []
        [(v,out)] -> [(f v, out)])

instance Applicative Parser where
    pure :: a -> Parser a
    pure v = P (\input -> [(v, input)])
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\input -> case parse pf input of
        [] -> []
        [(f,out)] -> parse (fmap f px) out)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\input -> case parse p input of
        [] -> []
        [(v,out)] -> parse (f v) out)

instance Alternative Parser where
    empty :: Parser a
    empty = P (\input -> [])
    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\input -> case parse p input of
        [] -> parse q input
        [(v,out)] -> [(v,out)])



item :: Parser Char
item = P (\input -> case input of
    [] -> []    
    (x:xs) -> [(x, xs)])

char :: Char -> Parser Char
char x = sat (== x)

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    rest <- string xs
    return (x:rest)

space :: Parser()
space = do
    many (sat isSpace)
    return ()

constant :: Parser Prop
constant = error "error"

var :: Parser Prop
var = error "error"

formula :: Parser Prop
formula = error "error"

parseFormula :: String -> String 
parseFormula s
    | [(result, "")] <- parse formula s = show result
    | otherwise = "Parse Error"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            contents <- readFile fileName
            let formulaStrings = lines contents
            mapM_ (putStrLn . parseFormula) formulaStrings