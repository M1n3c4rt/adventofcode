module Day03 where

import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/03.txt"
    -- part 1
    print $ sum $ map (\(x,y,z) -> z) $ filter (checkNeighbours (lines contents)) $ getNumbersWithCoords $ lines contents
    -- part 2
    print $ getGearSum $ foldr (flip (addGear (lines contents))) HM.empty $ getNumbersWithCoords $ lines contents

getNumbersWithCoords' :: Int -> [Char] -> [(Int, Int)]
getNumbersWithCoords' n ss = let (a,b) = break isDigit ss in
    if null b then [] else let (c,d) = span isDigit b in (n + length a, read c):getNumbersWithCoords' (n + length a + length c) d

getNumbersWithCoords :: [[Char]] -> [(Int, Int, Int)]
getNumbersWithCoords sss = concat $ zipWith (\ls y -> map (\(x,n) -> (x,y,n)) ls) (map (getNumbersWithCoords' 0) sss) [0..]

checkNeighbours :: [[Char]] -> (Int, Int, Int) -> Bool
checkNeighbours sss (x,y,n) = any (\(x,y) -> sss !! y !! x `notElem` "0123456789.") $ getNeighbours (x,y,n)

addGear :: [[Char]] -> HM.HashMap (Int,Int) (Int,Int) -> (Int, Int, Int) -> HM.HashMap (Int,Int) (Int,Int)
addGear sss m (x,y,n) = let gears = filter (\(x,y) -> sss !! y !! x == '*') $ getNeighbours (x,y,n) in
    foldl (\acc g -> HM.insertWith (\(num1,count1) (num2,count2) -> (num1*num2,count1+count2)) g (n,1) acc) m gears

getGearSum :: HM.HashMap (Int,Int) (Int,Int) -> Int
getGearSum = sum . HM.map fst . HM.filter (\(n,c) -> c > 1)

getNeighbours :: (Int, Int, Int) -> [(Int, Int)]
getNeighbours (x,y,n) = filter (\(x,y) -> min x y > -1 && max x y < 140) $ nub $ concatMap ((\(x,y) -> [(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x+1,y-1),(x-1,y+1),(x-1,y-1),(x+1,y+1)]) . (\k -> (x+k,y))) [0..length (show n)-1]