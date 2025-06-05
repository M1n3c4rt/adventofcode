module Day2019_05 where

import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Intcode

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/05.txt"
    -- part 1
    print $ last $ getOutput $ runC [1] $ parse contents
    -- part 2
    print $ last $ getOutput $ runC [5] $ parse contents