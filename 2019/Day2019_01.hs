module Day2019_01 where


main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/01.txt"
    -- part 1
    print $ sum $ map (subtract 2 . (`div` 3) . read) $ lines contents
    -- part 2
    print $ sum $ map (sum . tail . takeWhile (>0) . iterate (subtract 2 . (`div` 3)) . read) $ lines contents