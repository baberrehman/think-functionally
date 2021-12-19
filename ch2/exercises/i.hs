
import Data.Char

-- Exercise I

palindrome' :: String -> Bool
palindrome' xs = clean xs == reverse (clean xs)
                 where clean = map toLower . filter isAlpha

palindrome'' :: String -> String
palindrome'' xs
  | palindrome' xs == True = "Yes!"
  | otherwise              = "No!"

main = do  
    putStrLn "Enter a String:"  
    str <- getLine  
    putStrLn (palindrome'' str) 
