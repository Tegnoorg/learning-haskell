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

import Data.Char (isSpace, isLower, isAlphaNum, isDigit)
import Control.Applicative hiding (Const)
import System.Environment (getArgs)
import Prelude
import System.IO


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

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

ident :: Parser String
ident = do 
    x <- lower
    xs <- many alphanum
    return (x:xs)

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident 

symbol :: String -> Parser String
symbol xs = token (string xs)

constant :: Parser Prop
constant = true <|> false
  where
    true = do
        symbol "T"
        return (Const True)
    false = do
        symbol "F"
        return (Const False)


var :: Parser Prop
var = token (Var <$> identifier)

formula :: Parser Prop
formula = g1Formula
  where
    g1Formula = chain g1ImpTerm (symbol "<->" *> pure Iff)
    g1ImpTerm = chain g1AndTerm (symbol "->" *> pure Imply)
    g1AndTerm = chain g1OrTerm (symbol "/\\" *> pure And)
    g1OrTerm = chain g1Factor (symbol "\\/" *> pure Or)
    g1Factor = token (parens formula <|> constant <|> var)

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = do
  x <- p
  rest x
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y)
      <|> return x

parens :: Parser a -> Parser a
parens p = do
  symbol "("
  x <- p
  symbol ")"
  return x



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