module Day2022_10 where


main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/10.txt"
    let vals = parse 1 $ lines contents
    -- part 1
    print $ sum $ map (\n -> n*vals!!(n-2)) [20,60..220]
    -- part 2
    putStrLn $ tail $ draw 0 (1:init vals)

parse :: Int -> [String] -> [Int]
parse n (l:ls)
    | length l == 4 = n:parse n ls
    | otherwise = let added = (n+read (drop 5 l)) in n:added:parse added ls
parse n [] = []

draw :: Int -> [Int] -> String
draw n (v:vals) = (if n `mod` 40 == 0 then "\n" else "") ++ (if n `mod` 40 `elem` [v,v-1,v+1] then "##" else "..") ++ draw (n+1) vals
draw n [] = ""