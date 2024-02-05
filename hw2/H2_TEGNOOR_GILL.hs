myFoldl :: (a-> b -> a) -> a -> [b] -> a
myFoldl _ b [] = b
myFoldl a b (x:xs) = myFoldl a (a b x) xs

myFoldr :: (a-> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr a b (x:xs) = a x (myFoldr a b xs)

alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap _ _ [] = []
alternativeMap a b [x] = [a x] 
alternativeMap a b (x:y:xs)    = a x : b y : alternativeMap a b xs

myLength :: [a] -> Int
myLength = foldr (\_  x -> x + 1 ) 0

myFilter :: (a-> Bool) -> [a] -> [a]
myFilter a = foldr (\x b -> if a x then x : b else b) []

sumsqeven :: [Int] -> Int
sumsqeven = sum . map(^2) . filter even