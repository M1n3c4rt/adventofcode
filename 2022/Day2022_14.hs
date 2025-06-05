module Day2022_14 where

import qualified Data.Set as S
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/14.txt"
    let rocks = genRocks contents
        f = (+2) $ maximum $ S.map snd $ genRocks contents
    -- part 1
    print $ fall (500,0) rocks
    -- part 2
    print $ fall' f (500,0) rocks

type Rocks = S.Set (Int,Int)

genRocks :: String -> Rocks
genRocks = S.fromList . concatMap (helper' . map ((\[a,b] -> (read a,read b)) . splitOn ",") . splitOn " -> ") . lines
    where helper (a,b) (c,d) = if b == d then map (,b) [min a c..max a c] else map (a,) [min b d..max b d]
          helper' (x:y:ys) = helper x y ++ helper' (y:ys)
          helper' [x] = [x]

fall :: (Int,Int) -> Rocks -> Int
fall (x,y) rocks
    | y > 158 = 0
    | S.notMember (x,y+1) rocks = fall (x,y+1) rocks
    | S.notMember (x-1,y+1) rocks = fall (x-1,y+1) rocks
    | S.notMember (x+1,y+1) rocks = fall (x+1,y+1) rocks
    | otherwise = 1 + fall (500, 0) (S.insert (x,y) rocks)

fall' :: Int -> (Int,Int) -> Rocks -> Int
fall' f c@(x,y) rocks
    | S.member (500,0) rocks = 0
    | S.notMember (x,y+1) rocks && y+1 /= f = fall' f (x,y+1) rocks
    | S.notMember (x-1,y+1) rocks && y+1 /= f = fall' f (x-1,y+1) rocks
    | S.notMember (x+1,y+1) rocks && y+1 /= f = fall' f (x+1,y+1) rocks
    | otherwise = 1 + fall' f (500, 0) (S.insert (x,y) rocks)