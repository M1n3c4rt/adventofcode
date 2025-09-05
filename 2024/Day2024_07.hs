module Day2024_07 where

import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2024/07.txt"
    let numbers = map getNumbers $ lines contents
    -- part 1
    print $ sum $ map fst $ filter (\x -> fst x `elem` uncurry f x) numbers
    -- part 2
    print $ sum $ map fst $ filter (\x -> fst x `elem` uncurry g x) numbers
    
getNumbers :: String -> (Int, [Int])
getNumbers s = let [a,b] = splitOn ":" s in (read a, map read $ words b)

f :: Int -> [Int] -> [Int]
f n (x:y:ys) = concat $ filter (\x' -> null x' || head x' <= n) [f n ((x+y):ys),f n ((x*y):ys)]
f n [x] = [x]
f n [] = []

(~) :: Int -> Int -> Int
(~) x y = read $ show x ++ show y

g :: Int -> [Int] -> [Int]
g n (x:y:ys) = concat $ filter (\x' -> null x' || head x' <= n) [g n ((x+y):ys),g n ((x*y):ys),g n ((x~y):ys)]
g n [x] = [x]
g n [] = []