module Day2019_15 where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Intcode (parse, CompilerState, runC, getOutput)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/15.txt"
    let u@(a,b,c,d) = advance S.empty [(False,(0,0),0,parse contents)]
    -- part 1
    print c
    -- part 2
    print $ (\(a1,a2,a3,a6) -> a3) $ advance S.empty [(False,b,0,d)]

advance :: S.Set (Int,Int) -> [(Bool, (Int,Int), Int, CompilerState)] -> (Bool, (Int,Int), Int, CompilerState)
advance visited (u@(isTank,(a,b),n,state):unexplored)
    | isTank || (null unexplored && null filtered) = u
    | otherwise = advance (S.insert (a,b) visited) $ unexplored ++ filtered
    where
        neighbours = zipWith (\x point -> (point,) $ runC [x] state) [1,2,3,4] [(a,b+1),(a,b-1),(a+1,b),(a-1,b)]
        helper (point,s) = case getOutput s of
            [0] -> Nothing
            [1] -> if point `S.member` visited then Nothing else Just (False,point,n+1,s)
            [2] -> if point `S.member` visited then Nothing else Just (True,point,n+1,s)
        filtered = mapMaybe helper neighbours