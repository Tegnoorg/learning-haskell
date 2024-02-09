data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

listZip :: List a -> List b -> List (a, b)
listZip Empty _ = Empty
listZip _ Empty = Empty
listZip (Cons x  xs) (Cons y ys) = Cons (x,y) (listZip xs ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node x EmptyTree EmptyTree
insert x (Node a left right)
    | x == a = Node x left right
    | x > a = Node a left (insert x right)
    | x < a = Node a (insert x left) right

data Nat = Zero | Succ Nat deriving (Show)

natPlus :: Nat -> Nat -> Nat
natPlus Zero x = x
natPlus (Succ x) y = Succ (natPlus x y)

natMult :: Nat -> Nat -> Nat 
natMult Zero x = Zero
natMult (Succ x) y = natPlus (natMult x y) y

instance Eq a => Eq (Tree a) where
    (==) :: Eq a => Tree a -> Tree a -> Bool
    EmptyTree == EmptyTree = True 
    Node x l r == Node y ll rr = x == y && l == ll && r == rr
    _ == _ = False


data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where 
    fmap :: (a ->b ) -> AssocList k a -> AssocList k b
    fmap _ ALEmpty = ALEmpty
    fmap f (ALCons k v xs) = ALCons k (f v) (fmap f xs)