import Data.Foldable 

data ErrJst e j = Err e | Jst j deriving (Show)

instance Functor (ErrJst e) where
    fmap _ (Err e) = Err e
    fmap a (Jst j) = Jst (a j)

instance Applicative (ErrJst e) where
    pure = Jst
    Err e <*> _ = Err e
    Jst f <*> j = fmap f j

instance Monad (ErrJst e) where
    return = pure 
    (>>=) :: ErrJst e a -> (a -> ErrJst e b) -> ErrJst e b
    Err e >>= _ = Err e
    Jst f >>= j = j f

-- question 4


data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where 
    foldl f a (Leaf n) = f a n
    foldl f a (LNode l r) = foldl f (foldl f a l) r
    
    foldr f a (Leaf n) = f n a
    foldr f a (LNode l r) = foldr f (foldr f a r) l
