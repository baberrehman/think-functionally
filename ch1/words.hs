
import Data.List
import Data.Char

type Text = [Char]
type Word' = [Char]


-- words :: Text -> [Word]

-- toLower :: Char -> Char

-- map :: (a -> b) -> [a] -> [b]

-- map toLower :: Text -> Text

-- sortWords :: [Word'] -> [Word']
-- sortWords xs = sort xs

countRuns :: [Word'] -> [(Int, Word')]
countRuns []   = []
countRuns (x:xs) = let count = (length (filter (x == ) xs)) in
  [(count+1, x)] ++ (countRuns (drop count xs))

-- sortRuns :: [(Int, Word')] -> [(Int, Word')]
-- sortRuns = sortOn fst

showRun :: (Int, Word') -> String
showRun (n, w) = w ++ " " ++ show n ++ "\n"

commonWords :: Int -> Text -> [(Int, Word')]
commonWords n = take n . reverse .
                sortOn fst . countRuns . sort .
                words . map toLower
