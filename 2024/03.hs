import Data.List.Split ( splitOn )

main :: IO ()
main = do
    contents <- readFile "03.txt"
    -- part 1
    print $ sum $ map getTwoDigits $ mulsplit contents
    -- part 2
    print $ sum $ map getTwoDigits $ mulsplit $ spl contents
    
getNumbers :: String -> [Int]
getNumbers s = map read $ words s :: [Int]
spl :: String -> String
spl s = concatMap (head . splitOn "don't()") $ splitOn "do()" $ "do()"++s

mulsplit :: String -> [String]
mulsplit = splitOn "mul("

-- i am so sorry
getTwoDigits :: String -> Int
getTwoDigits s
    | (removeDigit s /= "") && (head (removeDigit s) `elem` ",") && (removeDigit (tail $ removeDigit s) /= tail (removeDigit s)) && (removeDigit (tail $ removeDigit s) /= "") && (head (removeDigit $ tail $ removeDigit s) `elem` ")") = read (getDigit s) * read (getDigit $ tail $ removeDigit s)
    | otherwise = 0

getDigit :: String -> String
getDigit (s:ss)
    | s `elem` "0123456789" = s : getDigit ss
    | otherwise             = ""
getDigit "" = ""

removeDigit :: String -> String
removeDigit (s:ss)
    | s `elem` "0123456789" = removeDigit ss
    | otherwise             = s:ss
removeDigit "" = ""