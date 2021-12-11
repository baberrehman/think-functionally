
double :: Integer -> Integer
double x = 2*x


sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs
