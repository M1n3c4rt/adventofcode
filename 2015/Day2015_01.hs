module Day2015_01 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/01.txt"
    -- part 1
    print $ foldl elevator 0 contents
    -- part 2
    print $ length $ takeWhile (>=0) $ scanl elevator 0 contents

elevator acc ')' = -1 + acc
elevator acc '(' = 1 + acc