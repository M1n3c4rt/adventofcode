module Day2025_08 where
import Data.List.Extra (splitOn, sortOn)
import Utility.AOC (choose)
import Data.Ord (Down(Down))
import Data.DisjointSet as DS ( empty, sets, toSets, union, values, DisjointSet )
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/08.txt"
    let dists = distMap $ map parse $ lines contents
    -- part 1
    print $ product $ take 3 $ sortOn Down $ map S.size $ toSets $ connectPart $ take 1000 dists
    -- part 2
    print $ (\((a,b,c),(d,e,f)) -> a*d) $ connectFull empty dists

type Point = (Int,Int,Int)

parse :: String -> Point
parse = (\[a,b,c] -> (a,b,c)) . map read . splitOn ","

dist :: Point -> Point -> Int
dist (a,b,c) (d,e,f) = (a-d)^2 + (b-e)^2 + (f-c)^2

distMap :: [Point] -> [(Point, Point)]
distMap ns = sortOn (uncurry dist) $ map (\[a,b] -> (a,b)) $ choose 2 ns

connectPart :: [(Point, Point)] -> DisjointSet Point
connectPart = foldl (\acc (a,b) -> union a b acc) empty

connectFull :: DisjointSet Point -> [(Point, Point)] -> (Point, Point)
connectFull current ((a,b):ds) = let new = union a b current in
    if sets new == 1 && values new == 1000 then (a,b) else
        connectFull new ds
