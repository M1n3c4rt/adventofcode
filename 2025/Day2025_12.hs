module Day2025_12 where
import Utility.AOC (numbers)
import Data.List.Extra (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/12.txt"
    -- part 1
    print $ length $ filter isPos $ parse contents
    -- part 2
    -- gg!

isPos :: (Int, Int, [Int]) -> Bool
isPos (a,b,bs) = sum (map (*9) bs) <= down a * down b
    where down k = k - (k `mod` 3)

parse :: [Char] -> [(Int, Int, [Int])]
parse c = map ((\(a:b:bs) -> (a,b,bs)) . numbers) $ lines $ last $ splitOn "\n\n" c