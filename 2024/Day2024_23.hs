module Day2024_23 where

import Data.List ( maximumBy, intercalate, intersect, nub, sort, union )
import Data.Function ( on )
import qualified Algebra.Graph.Undirected as G
import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2024/23.txt"
    let g = getGraph contents
    let cs = getMaxCliques g [] (G.vertexList g) []
    -- part 1
    print $ getTriangles (getGraph contents) (getTNodes contents)
    -- part 2
    putStrLn $ intercalate "," $ sort $ maximumBy (compare `on` length) cs

getGraph :: String -> G.Graph String
getGraph = G.edges . map ((\[x,y] -> (x,y)) . splitOn "-") . lines

getTriangles :: Ord a => G.Graph a -> [a] -> Int
getTriangles g ns = length $ nub $ concatMap (S.toList . S.map S.fromList) (zipWith (\a es -> S.map (\(x,y) -> [a,x,y]) es) ns (map (S.filter (\(a,b) -> G.hasEdge a b g) . choose2S . (`G.neighbours` g)) ns))

getMaxCliques :: G.Graph String -> [String] -> [String] -> [String] -> [[String]]
getMaxCliques g r [] [] = [r]
getMaxCliques g r' p x' = helper (if null p then "aa" else head p) r' p x'
    where helper u r (q:qs) x | q `S.member` G.neighbours u g = helper u r qs (q:x) | otherwise = getMaxCliques g (r `union` [q]) (p `intersect` S.toList (G.neighbours q g)) (x `intersect` S.toList (G.neighbours q g)) ++ helper u r qs (q:x); helper u r [] x = []
        
getNodes :: String -> [String]
getNodes = nub . concatMap (splitOn "-") . lines

getTNodes :: String -> [String]
getTNodes = filter ((=='t') . head) . getNodes

choose2 :: [a] -> [(a,a)]
choose2 (x:xs) = map (x,) xs ++ choose2 xs
choose2 [] = []

choose2S :: Ord a => S.Set a -> S.Set (a,a)
choose2S = S.fromList . choose2 . S.toList