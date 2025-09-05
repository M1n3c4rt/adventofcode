module Day2022_15 where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (sort, find)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/15.txt"
    let p = parse contents
    let r = rads p
    let is = map intercepts r
    -- part 1
    let Just m = fmap fst $ find (`insideRegion` r) $ map (,2000000) [-600000..]
    let Just n = fmap fst $ find (`insideRegion` r) $ map (,2000000) [5000000,4999999..]
    print $ n - m
    -- part 2
    let a = diff2s $ sort $ concatMap (\(a',b',c,d) -> [a',b']) is
    let b = diff2s $ sort $ concatMap (\(a',b',c,d) -> [c,d]) is
    print $ 4000000*((b-a)`div`2)+((b+a)`div`2)

parse :: String -> [((Int, Int), (Int, Int))]
parse = map ((\[_,a,b,c,d] -> ((a,b),(c,d))) . map (read . takeWhile (`elem` "0123456789-")) . splitOn "=") . lines

insideRegion :: (Int, Int) -> [((Int, Int), Int)] -> Bool
insideRegion q = any (\(p,r) -> taxicab p q <= r)

taxicab :: (Int, Int) -> (Int, Int) -> Int
taxicab (a,b) (c,d) = abs (a-c) + abs (b-d)

rads :: [((Int, Int), (Int, Int))] -> [((Int, Int), Int)]
rads = map (\(p,q) -> (p,taxicab p q))

bounds :: [((Int, Int), (Int, Int))] -> ((Int, Int), (Int, Int))
bounds coords =
    let top    = minimum $ map (\((a,b),(c,d)) -> b - taxicab (a,b) (c,d)) coords
        bottom = maximum $ map (\((a,b),(c,d)) -> b + taxicab (a,b) (c,d)) coords
        left   = minimum $ map (\((a,b),(c,d)) -> a - taxicab (a,b) (c,d)) coords
        right  = maximum $ map (\((a,b),(c,d)) -> a + taxicab (a,b) (c,d)) coords
    in ((left,top),(right,bottom))

borders :: ((Int, Int), Int) -> S.Set (Int,Int)
borders ((a,b),r') = let r = r' + 1 in S.fromList $ concatMap (\(c,y) -> [(a-r+abs c,y),(a+r-abs c,y)]) $ zip [-r..r] [b-r..b+r]

intercepts :: ((Int, Int), Int) -> (Int,Int,Int,Int)
intercepts ((a,b),r) = (b-a-r,b-a+r,a+b-r,a+b+r)

diff2s :: (Eq a, Num a) => [a] -> a
diff2s (x:y:xs)
    | y - x == 2 = x + 1
    | otherwise = diff2s (y:xs)