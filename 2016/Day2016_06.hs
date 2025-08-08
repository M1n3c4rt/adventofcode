module Day2016_06 where
import Data.List (sort, group, sortOn, transpose)
import Data.Ord (Down(Down))

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/06.txt"
    -- part 1
    putStrLn $ map mode $ transpose $ lines contents
    -- part 2
    putStrLn $ map mode' $ transpose $ lines contents

mode :: Ord a => [a] -> a
mode = head . head . sortOn (Down . length) . group . sort

mode' :: Ord a => [a] -> a
mode' = head . head . sortOn length . group . sort