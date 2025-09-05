module Day2018_25 where
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/25.txt"
    -- part 1
    let (s:stars) = parse contents
    print $ length $ consts [[]] [s] stars
    -- part 2
    -- gg!

parse :: String -> [[Int]]
parse = map (map read . splitOn ",") . lines

bordering :: [Int] -> [Int] -> Bool
bordering as bs = (<=3) $ sum $ zipWith (\x y -> abs (x-y)) as bs

consts :: [[[Int]]] -> [[Int]] -> [[Int]] -> [[[Int]]]
consts (f:finished) frontier stars
    | null newStars = (frontier++f):finished
    | null newFrontier = consts ([]:(frontier++f):finished) [s] rest
    | otherwise = consts ((frontier++f):finished) newFrontier newStars
    where
        newFrontier = filter (\s' -> any (bordering s') frontier) stars
        newStars = filter (`notElem` newFrontier) stars
        (s,rest) = (head newStars,tail newStars)