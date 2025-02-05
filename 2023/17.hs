import DijkstraSimple ( findShortestDistance, fromDistance, graphFromList )
import Data.List.Safe ((!!))
import Prelude hiding ((!!))
import Data.Maybe (fromMaybe)
import qualified Data.Bifunctor
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "17.txt"
    let graph = graphFromList $ getAllNodes getNodes $ lines contents
    let graph' = graphFromList $ getAllNodes getNodes' $ lines contents
    -- part 1
    print $ fromDistance $ findShortestDistance graph (0,0,0,1) (g,g,0,1)
    -- part 2
    print $ fromDistance $ findShortestDistance graph' (0,0,0,1) (g,g,0,1)

type Node = (Int,Int,Int,Int)

g :: Int
g = 140

(!?) :: [[Char]] -> (Int,Int) -> Int
ls !? (y,x) = read [fromMaybe '0' (fromMaybe [] (ls !! y) !! x)]

inBounds :: (Int, Int) -> Bool
inBounds (a,b) = min a b >= 0 && max a b < g+1

getNodes :: (Int, Int) -> [[Char]] -> [(Node, [(Node, Int)])]
getNodes (x,y) grid = let
    left = [((x-n,y,-1,0),sum $ map (\k -> grid !? (y,x-k)) [1..n]) | n <- [1..3]]
    right = [((x+n,y,1,0),sum $ map (\k -> grid !? (y,x+k)) [1..n]) | n <- [1..3]]
    up = [((x,y-n,0,-1),sum $ map (\k -> grid !? (y-k,x)) [1..n]) | n <- [1..3]]
    down = [((x,y+n,0,1),sum $ map (\k -> grid !? (y+k,x)) [1..n]) | n <- [1..3]] in
        map (Data.Bifunctor.second (filter (\ ((x, y, a, b), n) -> inBounds (x, y)))) [((x,y,-1,0),up ++ down),
        ((x,y,1,0),up ++ down),
        ((x,y,0,-1),left ++ right),
        ((x,y,0,1),left ++ right)]
    
getNodes' :: (Int, Int) -> [[Char]] -> [(Node, [(Node, Int)])]
getNodes' (x,y) grid = let
    left = [((x-n,y,-1,0),sum $ map (\k -> grid !? (y,x-k)) [1..n]) | n <- [4..10]]
    right = [((x+n,y,1,0),sum $ map (\k -> grid !? (y,x+k)) [1..n]) | n <- [4..10]]
    up = [((x,y-n,0,-1),sum $ map (\k -> grid !? (y-k,x)) [1..n]) | n <- [4..10]]
    down = [((x,y+n,0,1),sum $ map (\k -> grid !? (y+k,x)) [1..n]) | n <- [4..10]] in
        map (Data.Bifunctor.second (filter (\ ((x, y, a, b), n) -> inBounds (x, y)))) [((x,y,-1,0),up ++ down),
        ((x,y,1,0),up ++ down),
        ((x,y,0,-1),left ++ right),
        ((x,y,0,1),left ++ right)]

getAllNodes :: ((Int, Int) -> t -> [a]) -> t -> [a]
getAllNodes f grid = concat [f (x,y) grid | x <- [0..g], y <- [0..g]]