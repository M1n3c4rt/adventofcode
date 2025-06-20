module Day2019_09 where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Intcode ( parse, runC, getOutput ) 

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/09.txt"
    -- part 1
    print $ head $ getOutput $ runC [1] $ parse contents
    -- part 2
    print $ head $ getOutput $ runC [2] $ parse contents