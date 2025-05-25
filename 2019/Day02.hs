module Day02 where

import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Control.Monad ((>=>))
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/02.txt"
    -- part 1
    print $ fromJust $ HM.lookup 0 $ run 0 $ HM.insert 1 12 $ HM.insert 2 2 $ parse contents
    -- part 2
    print $ (\(x,y,_) -> 100*x+y) $ head $ filter (\(x,y,state) -> fromJust (HM.lookup 0 $ run 0 state) == 19690720) $ getCombos $ parse contents

parse :: [Char] -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

run :: Int -> HM.HashMap Int Int -> HM.HashMap Int Int
run pointer state = case HM.lookup pointer state of
    Just 1 -> run (pointer+4) $ HM.insert c (a+b) state
    Just 2 -> run (pointer+4) $ HM.insert c (a*b) state
    Just 99 -> state
    where
        a = fromJust $ HM.lookup (fromJust $ HM.lookup (pointer+1) state) state
        b = fromJust $ HM.lookup (fromJust $ HM.lookup (pointer+2) state) state
        Just c = HM.lookup (pointer+3) state

getCombos :: HM.HashMap Int Int -> [(Int,Int,HM.HashMap Int Int)]
getCombos state = [(x,y,HM.insert 1 x $ HM.insert 2 y state) | x <- [0..99], y <- [0..99]]