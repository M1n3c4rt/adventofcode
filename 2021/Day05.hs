module Day05 where

import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2021/05.txt"
    -- part 1
    print $ HM.size $ HM.filter (>1) $ parseWith helper contents
    -- part 2
    print $ HM.size $ HM.filter (>1) $ parseWith helper' contents

parseWith :: (String -> [(Int,Int)]) -> String -> HM.HashMap (Int,Int) Int
parseWith f = foldr (\c acc -> HM.insertWith (+) c 1 acc) HM.empty . concatMap f . lines

helper :: String -> [(Int,Int)]
helper line =
    let [[a,b],[c,d]] = map (map read . splitOn ",") $ (\[a,b,c] -> [a,c]) $ words line
    in
        if a /= c && b /= d then [] else
            if a == c then map (a,) [min b d .. max b d] else
                map (,b) [min a c .. max a c]

helper' :: String -> [(Int,Int)]
helper' line =
    let [[a,b],[c,d]] = map (map read . splitOn ",") $ (\[a,b,c] -> [a,c]) $ words line
    in
        if a /= c && b /= d then zip [min a c .. max a c] $ (if (b < d) == (a < c) then id else reverse) [min b d .. max b d] else
            if a == c then map (a,) [min b d .. max b d] else
                map (,b) [min a c .. max a c]