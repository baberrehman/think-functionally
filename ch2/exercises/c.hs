
import Data.Char

-- Exercise C

toUpperList :: String -> String
toUpperList [] = []
toUpperList (x:xs) = (toUpper x) : xs

modernise :: String -> String
modernise = unwords . map toUpperList . words

-- Exercise D

first :: (a -> Bool) -> [a] -> a
first p xs
  | null xs = error "Empty list"
  | p x = x
  | otherwise = first p (tail xs)
  where x = head xs
