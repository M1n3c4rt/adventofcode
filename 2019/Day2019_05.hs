module Day2019_05 where

import Intcode ( parse, runC, getOutput )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/05.txt"
    -- part 1
    print $ last $ getOutput $ runC [1] $ parse contents
    -- part 2
    print $ last $ getOutput $ runC [5] $ parse contents