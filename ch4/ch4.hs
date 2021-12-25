
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : (iterate' f x)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' xs = sum $ [1 | x <- xs]

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y): zip' xs ys
zip' _ _ = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

