
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : (iterate' f x)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' xs = sum $ [1 | x <- xs]

