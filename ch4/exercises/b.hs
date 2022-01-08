
-- Exerceise B

allpairs = [(x,y) | x <- [0..], y <- [0..]]

allpairs' = [(x,y) | x <- [0..], y <- [x..]]

-- Exercise C

disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs ys = (length [x | x <- xs , x `elem` ys]) > 0

-- Exercise E

quadruples :: Int -> Int -> [((Int,Int),(Int,Int))]
quadruples n m = take n [((x1,x2),(y1,y2)) | x1 <- [1..m], x2 <- [x1+1..m], y1 <- [1..m], y2 <- [y1+1..m], x1 /= y1, x2 /= y2, x1*x1*x1 + x2*x2*x2 == y1*y1*y1 + y2*y2*y2]

-- Exercise F

data List a = Nil | Snoc (List a) a

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = (Snoc (toList xs) x) 

fromList :: List a -> [a]
fromList Nil = []
fromList (Snoc l x) = (fromList l) ++ [x]

head' :: List a -> a
head' l = head (fromList l)

last' :: List a -> a
last' (Snoc _ x) = x

-- Exercise H

take' :: Integer -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : (take' (n-1) xs)

drop' :: Integer -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

splitAt' :: Integer -> [a] -> ([a],[a])
splitAt' 0 xs = ([],xs)
splitAt' n (x:xs) = let l = splitAt' (n-1) xs in
  (x : (fst l), (snd l))

-- Exercise I
