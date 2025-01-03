import DijkstraSimple ( findShortestDistance, fromDistance, graphFromList )
import Data.List.Split (splitOn)
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "18.txt"
    let b = getBlocks contents
    let g = getGraph (take 1024 $ getBlocks contents) (genGrid 70 70)
    -- part 1
    print $ fromDistance $ findShortestDistance (graphFromList g) (0,0) (70,70)
    -- part 2
    print $ firstblock (drop 1024 b) g

type Node = (Int,Int)

getBlocks :: String -> [(Int,Int)]
getBlocks s = map (\x -> let [a,b] = splitOn "," x in (read a, read b)) $ lines s

firstblock :: [Node] -> [(Node, [(Node, Int)])] -> Node
firstblock (r:remblocks) graph
    | (==(-1)) $ fromDistance $ findShortestDistance (graphFromList newgraph) (0,0) (70,70) = r
    | otherwise = firstblock remblocks newgraph
    where newgraph = updateGraph r graph

genGrid :: Int -> Int -> [(Int,Int)]
genGrid m n = concatMap (zip [0..m] . repeat) [0..n]

updateGraph :: Node -> [(Node, [(Node, Int)])] -> [(Node, [(Node, Int)])]
updateGraph block graph = map (Data.Bifunctor.second (filter (\ (p, q) -> p /= block))) $ filter (\(a,b) -> a /= block) graph

getGraph :: [Node] -> [Node] -> [(Node, [(Node, Int)])]
getGraph blocks grid = map (\(x,y) -> ((x,y), map (,1) $ filter (\t -> t `elem` grid && t `notElem` blocks) [(x,y+1),(x,y-1),(x+1,y),(x-1,y)])) $ filter (`notElem` blocks) grid