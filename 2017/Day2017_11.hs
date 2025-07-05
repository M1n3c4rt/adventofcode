module Day2017_11 where
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/11.txt"
    -- part 1
    print $ mixedTaxi (0,0) $ foldl add2 (0,0) $ map dirmap $ splitOn "," contents
    -- part 2
    print $ maximum $ map (mixedTaxi (0,0)) $ scanl add2 (0,0) $ map dirmap $ splitOn "," contents

dirmap s = case s of
    "n" -> (0,1)
    "s" -> (0,-1)
    "ne" -> (1,1)
    "se" -> (1,0)
    "nw" -> (-1,0)
    "sw" -> (-1,-1)

mixedTaxi (a,b) (x,y)
    | b == y = abs (a-x)
    | a == x = abs (b-y)
    | x > a && y > b = max (abs (a-x)) (abs (b-y))
    | x > a && y < b = abs (a-x) + abs (b-y)
    | otherwise = mixedTaxi (x,y) (a,b)

add2 (a,b) (c,d) = (a+c,b+d)