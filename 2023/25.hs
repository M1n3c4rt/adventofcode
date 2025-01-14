import Data.List.Split (splitOn)
import DijkstraSimple ( findShortestDistance, Distance(Infinity), Graph, findShortestPaths )
import Data.Function (on)
import Data.List (maximumBy, sortOn, delete, nub)
import qualified Data.HashMap.Strict as HM
import Prelude hiding (init)

main :: IO ()
main = do
    contents <- readFile "25.txt"
    let graph = getGraph contents
    let nodes = getNodes contents
    let c = choose2 nodes
    -- part 1
    print $ getGraphMultSize nodes graph

    -- got the likely candidates for bridges using the below line, and disconnected the graph manually
    --print $ mode $ map (uncurry (findShortestPaths graph)) $ take 200 c

    -- part 2
    -- gg!

getGraph :: String -> Graph String
getGraph = graphFromEdges . concatMap ((\[a,bs] -> map (a,) $ splitOn " " bs) . splitOn ": ") . lines

graphFromEdges :: [(String,String)] -> Graph String
graphFromEdges = foldl (\acc (a,b) -> HM.insertWith (++) a [(b,1)] $ HM.insertWith (++) b [(a,1)] acc) HM.empty

getNodes :: String -> [String]
getNodes = nub . concatMap ((\[a,bs] -> a:splitOn " " bs) . splitOn ": ") . lines

choose2 :: [a] -> [(a,a)]
choose2 (x:xs) = map (x,) xs ++ choose2 xs
choose2 [] = []

pairUp :: [[([String], Distance Int)]] -> [(String,String)]
pairUp = concat . concatMap (map (map (\(a,b) -> (min a b, max a b)) . (\x -> zip x $ tail x) . fst))

accumulate :: [(String,String)] -> HM.HashMap (String,String) Int
accumulate = foldr (\c acc -> HM.insertWith (+) c 1 acc) HM.empty

mode :: [[([String], Distance Int)]] -> [((String, String), Int)]
mode = sortOn (negate . snd) . HM.toList . accumulate . pairUp

isDisconnected :: [String] -> Graph String -> Bool
isDisconnected nodes graph = elem Infinity $ map (uncurry $ findShortestDistance graph) $ choose2 nodes

getGraphMultSize :: [String] -> Graph String -> Int
getGraphMultSize nodes graph =
    let oneHalf = length $ filter (/= Infinity) $ map (findShortestDistance graph (head nodes)) nodes
    in (length nodes - oneHalf)*oneHalf