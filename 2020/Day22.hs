module Day22 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/22.txt"
    -- part 1
    print $ uncurry play $ parse contents
    -- part 2
    print $ uncurry (playRec S.empty) $ parse contents

parse :: [Char] -> ([Int], [Int])
parse = (\[a,b] -> (a,b)) . map (map read . tail . lines) . splitOn "\n\n"

play (x:xs) (y:ys)
    | x > y = play (xs ++ [x,y]) ys
    | y > x = play xs (ys ++ [y,x])
play [] ys = sum $ zipWith (*) [1..] $ reverse ys
play xs [] = sum $ zipWith (*) [1..] $ reverse xs

playRec table (x:xs) (y:ys)
    | (x:xs,y:ys) `S.member` table = (True,sum $ zipWith (*) [1..] $ reverse ys)
    | x <= length xs && y <= length ys =
        if fst $ playRec S.empty (take x xs) (take y ys) then
            playRec (S.insert (x:xs,y:ys) table) (xs ++ [x,y]) ys
        else
            playRec (S.insert (x:xs,y:ys) table) xs (ys ++ [y,x])
    | x > y = playRec (S.insert (x:xs,y:ys) table) (xs ++ [x,y]) ys
    | y > x = playRec (S.insert (x:xs,y:ys) table) xs (ys ++ [y,x])
playRec _ [] ys = (False,sum $ zipWith (*) [1..] $ reverse ys)
playRec _ xs [] = (True,sum $ zipWith (*) [1..] $ reverse xs)