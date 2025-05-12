module Day15 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/15.txt"
    let ns = map read $ splitOn "," contents
    -- part 1
    print $ (!!(2020-length ns-1)) $ next $ initialize ns
    -- part 2
    print $ (!!(30000000-length ns-1)) $ next $ initialize ns
    

initialize :: [Int] -> (HM.HashMap Int Int,Int,Int)
initialize ls = (HM.fromList $ zip ls [1..],0,length ls + 1)

next :: (HM.HashMap Int Int,Int,Int) -> [Int]
next (nums,n,turn) = case HM.lookup n nums of
    Nothing -> n:next (HM.insert n turn nums,0,turn+1)
    Just k -> n:next (HM.insert n turn nums,turn-k,turn+1)