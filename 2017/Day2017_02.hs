module Day2017_02 where
import Utility.AOC (permute)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/02.txt"
    -- part 1
    print $ sum $ map (\x -> maximum x - minimum x) $ parse contents
    -- part 2
    print $ sum $ map (head . map (\[a,b] -> a `div` b) . filter (\[a,b] -> a `mod` b == 0) . permute 2) $ parse contents

parse :: String -> [[Int]]
parse = map (map read . words) . lines