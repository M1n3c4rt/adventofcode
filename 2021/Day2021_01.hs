module Day2021_01 where


main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2021/01.txt"
    -- part 1
    print $ length $ filter (>0) $ diffList $ parse contents
    -- part 2
    print $ length $ filter (>0) $ diffList $ sumList $ parse contents

parse :: String -> [Int]
parse = map read . lines

diffList :: [Int] -> [Int]
diffList (x:y:xs) = (y-x):diffList (y:xs)
diffList [x] = []

sumList :: [Int] -> [Int]
sumList (a:b:c:xs) = (a+b+c):sumList (b:c:xs)
sumList [a,b] = []