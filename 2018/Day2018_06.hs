module Day2018_06 where
import qualified Data.Set as S
import Data.Function (on)
import Utility.AOC (taxicab2, floodFillWith, neighbours4)
import Data.Foldable (minimumBy, maximumBy)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/06.txt"
    let points = S.fromList $ map parse $ lines contents
    let boundary = S.map (`closestPoint` points) $ border $ bounds points
    let middle = (\((a,b),(c,d)) -> ((a+c)`div`2,(b+d)`div`2)) $ bounds points
    -- part 1
    print $ maximum $ S.map S.size $ S.map (\x -> floodFill points [x] S.empty) $ S.filter (`S.notMember` boundary) points
    -- part 2
    print $ S.size $ floodFill' points [middle] S.empty

parse :: String -> (Int,Int)
parse line = read $ "(" ++ line ++ ")"

bounds :: S.Set (Int,Int) -> ((Int,Int),(Int,Int))
bounds points = ((minimum $ S.map fst points, minimum $ S.map snd points),(maximum $ S.map fst points, maximum $ S.map snd points))

closestPoint :: (Int,Int) -> S.Set (Int,Int) -> (Int,Int)
closestPoint point = minimumBy (compare `on` taxicab2 point)

border :: ((Int,Int),(Int,Int)) -> S.Set (Int,Int)
border ((a,b),(c,d)) = S.fromList $ map (,b-5) [a-5..c+4] ++ map (c+5,) [b-5..d+4] ++ map (,d+5) [a-4..c+5] ++ map (a-5,) [b-4..d+5]

floodFill :: S.Set (Int, Int) -> [(Int, Int)] -> S.Set (Int, Int) -> S.Set (Int, Int)
floodFill points (f:frontier) finished = floodFill points (frontier++filter (\p -> p `notElem` frontier && p `notElem` finished && ((==) `on` (`closestPoint` points)) p f) (neighbours4 f)) (S.insert f finished)
floodFill points [] finished = finished

floodFill' :: S.Set (Int, Int) -> [(Int, Int)] -> S.Set (Int, Int) -> S.Set (Int, Int)
floodFill' points (f:frontier) finished = floodFill' points (frontier++filter (\p -> p `notElem` frontier && p `notElem` finished && isClose p points) (neighbours4 f)) (S.insert f finished)
floodFill' points [] finished = finished

isClose :: (Int,Int) -> S.Set (Int,Int) -> Bool
isClose point = (<10000) . sum . map (taxicab2 point) . S.toList