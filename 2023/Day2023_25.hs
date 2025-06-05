module Day2023_25 where

import Data.List.Split (splitOn)
import Utility.AOC ( shortestDistance )
import Data.Function (on)
import Data.List (maximumBy, sortOn, delete, nub)
import qualified Data.HashMap.Strict as HM
import Prelude hiding (init)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/25.txt"
    let graph = getGraph contents
    let nodes = getNodes contents
    let c = choose2 nodes
    -- part 1
    print $ getGraphMultSize nodes graph

    -- got the likely candidates for bridges using the below line, and disconnected the graph manually
    --print $ mode $ map (uncurry (findShortestPaths graph)) $ take 200 c

    -- part 2
    -- gg!

type Graph = HM.HashMap [Char] [([Char], Integer)]

getGraph :: String -> Graph
getGraph = graphFromEdges . concatMap ((\[a,bs] -> map (a,) $ splitOn " " bs) . splitOn ": ") . lines

graphFromEdges :: [([Char], [Char])] -> Graph
graphFromEdges = foldl (\acc (a,b) -> HM.insertWith (++) a [(b,1)] $ HM.insertWith (++) b [(a,1)] acc) HM.empty

getNodes :: String -> [String]
getNodes = nub . concatMap ((\[a,bs] -> a:splitOn " " bs) . splitOn ": ") . lines

choose2 :: [a] -> [(a,a)]
choose2 (x:xs) = map (x,) xs ++ choose2 xs
choose2 [] = []

pairUp :: [[([String], Maybe Int)]] -> [(String,String)]
pairUp = concat . concatMap (map (map (\(a,b) -> (min a b, max a b)) . (\x -> zip x $ tail x) . fst))

accumulate :: [(String,String)] -> HM.HashMap (String,String) Int
accumulate = foldr (\c acc -> HM.insertWith (+) c 1 acc) HM.empty

mode :: [[([String], Maybe Int)]] -> [((String, String), Int)]
mode = sortOn (negate . snd) . HM.toList . accumulate . pairUp

isDisconnected :: [String] -> Graph -> Bool
isDisconnected nodes graph = elem Nothing $ map (uncurry $ shortestDistance graph) $ choose2 nodes

getGraphMultSize :: [String] -> Graph -> Int
getGraphMultSize nodes graph =
    let oneHalf = length $ filter (/= Nothing) $ map (shortestDistance graph (head nodes)) nodes
    in (length nodes - oneHalf)*oneHalf