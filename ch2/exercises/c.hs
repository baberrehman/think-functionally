
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

-- Exercise E

first' :: (a -> Bool) -> [a] -> Maybe a
first' p xs
  | null xs = Nothing
  | p x = Just x
  | otherwise = first' p (tail xs)
  where x = head xs


-- Exercise G

type Date = (Int,Int,Int)

months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

dayPostFix :: Int -> String
dayPostFix x
  | xmod == 1 && x /= 11 = show x ++ "st"
  | xmod == 2 && x /= 12 = show x ++ "nd"
  | xmod == 3 && x /= 13 = show x ++ "rd"
  | otherwise = show x ++ "th"
  where xmod = x `mod` 10
  

showDate :: Date -> String
showDate (day, month, year) = dayPostFix day ++ " " ++ months !! (month-1) ++ ", " ++ show year
