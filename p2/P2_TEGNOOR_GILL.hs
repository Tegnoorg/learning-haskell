import Data.Char (isSpace)

data Prop = Const Bool
        | Var String
        | Not Prop
        | And Prop Prop
        | Or Prop Prop
        | Imply Prop Prop
        | Iff Prop Prop
        deriving (Eq, Read, Show)

