fib::Int->Int
fib n 
    | n<1 = 0
    | n == 1 = 1
    | otherwise = fib(n-1) + fib (n-2)

listReverse::[a] -> [a]
listReverse [] = []
listReverse x = last x : listReverse (init x)

listAdd:: [Int] -> [Int] -> [Int]
listAdd [] [] = []
listAdd [] n = n
listAdd m [] = m
listAdd (x:xs) (y:ys) = x+y : listAdd xs ys

inList:: (Eq a) =>[a] -> a -> Bool
inList [] _ = False
inList (x:xs) y 
    | x == y = True
    | otherwise = inList xs y


sumTailRec::Num a=> [a] -> a
sumTailRec [] = 0
sumTailRec x = sumTailRecAux x 0
    where 
        sumTailRecAux :: Num a=> [a] -> a -> a
        sumTailRecAux [] n = n
        sumTailRecAux (x:xs) n = sumTailRecAux xs (n+x)