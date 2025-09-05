module Day2015_17 where
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.List.Extra (groupOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/17.txt"
    -- part 1
    print $ length $ combos (map read $ lines contents) 150
    -- part 2
    print $ length $ head $ groupOn length $ sortOn length $ combos (map read $ lines contents) 150

combos :: [Int] -> Int -> [[Int]]
combos containers 0 = [[]]
combos containers n = let as = init $ scanr (:) [] $ dropWhile (>n) $ sortOn Down containers in
        concatMap (\(n':ns) -> map (n':) $ combos ns (n-n')) as