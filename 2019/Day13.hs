module Day13 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Intcode ( CompilerState(NeedsInput), parse, runC, modify, getOutput )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/13.txt"
    let out = getOutput $ runC [] $ parse contents
    let outP = runC (repeat 0) $ modify 0 2 $ cheat $ parse contents
    -- part 1
    print $ length $ filter (==2) $ takeEvery 3 out
    -- part 2
    print $ last $ getOutput outP

takeEvery :: Int -> [a] -> [a]
takeEvery n ns = case drop (n-1) ns of
    b:bs -> b:takeEvery n bs
    [] -> []

cheat :: CompilerState -> CompilerState
cheat (NeedsInput (a,b,c,d)) = NeedsInput (a,b,foldr (`HM.insert` 3) c [1585..1627],d)