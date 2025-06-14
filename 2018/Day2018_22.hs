module Day2018_22 where
import Data.MemoUgly (memo)
import qualified Data.HashMap.Strict as HM
import Utility.AOC (neighbours4, shortestDistance)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/22.txt"
    let (depth,(p,q)) = parse contents
        cave = [if (x,y) /= (p,q) then ((x,y),getType (depth,(x,y))) else ((x,y),0) | x <- [0..p], y <- [0..q]]
        cave' = [if (x,y) /= (p,q) then ((x,y),getType (depth,(x,y))) else ((x,y),0) | x <- [0..4*p], y <- [0..4*q]]
    -- part 1
    print $ sum $ map snd cave
    -- part 2
    print $ fromJust $ shortestDistance (genGraph cave') ((0,0),1) ((p,q),1)

parse :: String -> (Int,(Int,Int))
parse contents = let [l1,l2] = lines contents in (read $ drop 7 l1,read $ "("++drop 8 l2++")")

getErosion :: (Int,(Int,Int)) -> Int
getErosion (depth,(0,0)) = (0+depth) `mod` 20183
getErosion (depth,(0,y)) = (48271*y+depth) `mod` 20183
getErosion (depth,(x,0)) = (x*16807+depth) `mod` 20183
getErosion (depth,(x,y)) = (getErosionMemo (depth,(x-1,y)) * getErosionMemo (depth,(x,y-1)) + depth) `mod` 20183

getErosionMemo :: (Int, (Int, Int)) -> Int
getErosionMemo = memo getErosion

getType :: (Int, (Int, Int)) -> Int
getType = (`mod` 3) . getErosionMemo

genGraph :: [((Int,Int),Int)] -> HM.HashMap ((Int,Int),Int) [(((Int,Int),Int),Int)]
genGraph cave = HM.fromList $ concatMap helper cave
    where
        a -% b = (a-b) `mod` 3
        a +% b = (a+b) `mod` 3
        cave' = HM.fromList cave
        t p = HM.lookupDefault (-1) p cave'
        helper (p,c) =
            [
                ((p,c-%1),(((p,c+%1),7):) $ map ((,1) . (,c-%1)) (filter ((/=c-%1) . t) $ filter (`HM.member` cave') $ neighbours4 p)),
                ((p,c+%1),(((p,c-%1),7):) $ map ((,1) . (,c+%1)) (filter ((/=c+%1) . t) $ filter (`HM.member` cave') $ neighbours4 p))
            ]