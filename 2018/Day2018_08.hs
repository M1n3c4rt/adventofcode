module Day2018_08 where
import Data.Maybe (mapMaybe)
import Data.List.Safe ((!!))
import Prelude hiding ((!!))

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/08.txt"
    -- part 1
    print $ sumMetadata $ fst $ parseTree $ map read $ words contents
    -- part 2
    print $ sumValues $ fst $ parseTree $ map read $ words contents

newtype Tree = Tree ([Tree],[Int]) deriving Show

parseTree :: [Int] -> (Tree,[Int])
parseTree (n:m:xs) = let (trees,remaining) = parseTrees n xs in (Tree (trees,take m remaining),drop m remaining)
    where
        parseTrees 0 rem = ([],rem)
        parseTrees n [] = ([],[])
        parseTrees n ns = let (t,rem) = parseTree ns; (ts,rems) = parseTrees (n-1) rem in (t:ts,rems)

sumMetadata :: Tree -> Int
sumMetadata (Tree (ts,m)) = sum m + sum (map sumMetadata ts)

sumValues :: Tree -> Int
sumValues (Tree (ts,m))
    | null ts = sum m
    | otherwise = sum $ map sumValues $ mapMaybe ((ts!!) . (subtract 1)) m