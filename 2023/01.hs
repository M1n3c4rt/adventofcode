import Data.Char (isDigit)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    contents <- readFile "01.txt"
    -- part 1
    print $ sum $ map getNumberDigits $ lines contents
    -- part 2
    print $ sum $ map getWordDigits $ lines contents

getNumberDigits :: String -> Int
getNumberDigits s = read $ map (head . dropWhile (not . isDigit)) $ [reverse,id] <*> pure s

wordDigits :: [String]
wordDigits = ["0","1","2","3","4","5","6","7","8","9","one","two","three","four","five","six","seven","eight","nine"]

mapWordDigits :: String -> Char
mapWordDigits s = (fst . head) $ filter ((==s) . snd) $ zip ['0','1','2','3','4','5','6','7','8','9','1','2','3','4','5','6','7','8','9'] wordDigits

getWordDigits :: String -> Int
getWordDigits s = read $ [head,last] <*> pure (allPrefixes s)

allPrefixes :: String -> [Char]
allPrefixes [] = []
allPrefixes s@(x:xs)
    | null prefixes = allPrefixes xs
    | otherwise = mapWordDigits (head prefixes):allPrefixes xs
    where prefixes = filter (`isPrefixOf` s) wordDigits