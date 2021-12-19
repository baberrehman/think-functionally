
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

-- Exercise H

type CIN = String

addSum :: CIN -> CIN
addSum xs
  | (length xs) /= 8 = error "length is not equal to 8"
  | otherwise = xs ++ show (sum (map digitToInt xs))


valid :: CIN -> Bool
valid xs = lastTwo == (sumEight (take 8 xs))
           where lastTwo = (digitToInt (xs !! 8) * 10) + (digitToInt (xs !! 9))
                 sumEight xs' = sum (map digitToInt xs')
