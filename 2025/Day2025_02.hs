module Day2025_02 where
import Data.List.Extra (splitOn, chunksOf, nub)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/02.txt"
    -- part 1
    print $ solveWith invalid contents
    -- part 2
    print $ solveWith invalid' contents

invalid :: String -> Bool
invalid s = take (length s `div` 2) s == drop (length s `div` 2) s

invalid' :: String -> Bool
invalid' s = any (\k -> (==1) $ length $ nub $ chunksOf (length s `div` k) s) [2..length s `div` 2]

solveWith :: (String -> Bool) -> [Char] -> Int
solveWith f contents = sum $ concatMap (filter (f . show) . (\[a,b] -> [read a..read b]) . splitOn "-") (splitOn "," contents)