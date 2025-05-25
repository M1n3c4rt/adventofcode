module Day02 where

import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Intcode ( CompilerState(Finished), parse, runC, modify, getState )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/02.txt"
    -- part 1
    print $ fromJust $ HM.lookup 0 $ getState $ runC [] $ modify 1 12 $ modify 2 2 $ parse contents
    -- part 2
    print $
        (\(x,y,_) -> 100*x+y) $ head $
            filter (\(x,y,state) -> fromJust (HM.lookup 0 $ getState $ runC [] state) == 19690720) $
                getCombos $ parse contents

getCombos :: CompilerState -> [(Int, Int, CompilerState)]
getCombos state = [(x,y,modify 1 x $ modify 2 y state) | x <- [0..99], y <- [0..99]]