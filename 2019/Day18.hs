module Day18 where

import Utility.AOC (enumerate, neighbours4)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Char (isLower, isUpper, toLower)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.MemoUgly (memo)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/18.txt"
    let grid = map (\(a,b,c) -> ((a::Int,b::Int),c)) $ enumerate contents
        graph = HM.fromList $ map (\(ns,c) -> (c,reachables S.empty [(ns,c,0)] $ HM.fromList grid)) $ findItems grid
        moddedGrid = mods ++ filter ((`notElem` [(x,y) | x <- [39..41], y <- [39..41]]) . fst) grid
        moddedGraph = HM.fromList $ map (\(ns,c) -> (c,reachables S.empty [(ns,c,0)] $ HM.fromList moddedGrid)) $ findItems moddedGrid
    -- part 1
    print $ navigateMemo ([],HM.filterWithKey (\k _ -> not (isUpper k)) $ HM.mapWithKey (\k _ -> flatten [k] k graph) graph,'@')
    -- part 2
    print $ navigateMultipleMemo ([],HM.filterWithKey (\k _ -> not (isUpper k)) $ HM.mapWithKey (\k _ -> flatten [k] k moddedGraph) moddedGraph,"1234")

findItems :: [(a, Char)] -> [(a, Char)]
findItems = filter ((`notElem` ".#") . snd)

reachables :: S.Set (Int,Int) -> [((Int,Int),Char,Int)] -> HM.HashMap (Int,Int) Char -> HM.HashMap Char Int
reachables finished ((ns,c,s):unfinished) grid
    | c == '#' = reachables (S.insert ns finished) unfinished grid
    | not (null finished) && c `notElem` ".@#1234" = HM.insert c s $ reachables (S.insert ns finished) unfinished grid
    | otherwise =
        let newunfinished =
                unfinished ++ map (\x -> (x,HM.lookupDefault '#' x grid,s+1)) (filter (not . (`S.member` finished)) $ neighbours4 ns)
        in reachables (S.insert ns finished) newunfinished grid
reachables finished [] grid = HM.empty

flatten :: String -> Char -> HM.HashMap Char (HM.HashMap Char Int) -> HM.HashMap Char (String,Int)
flatten finished key graph =
    let Just adjacents = HM.lookup key graph
        keys = HM.map ([],) $ HM.filterWithKey (\k _ -> isLower k && k `notElem` finished) adjacents
        filtered = HM.filterWithKey (\k _ -> k `notElem` finished) adjacents
        rest = foldr (HM.unionWith min) HM.empty $ HM.elems $ HM.mapWithKey (\d n -> HM.map (bimap (if isUpper d then (d:) else id) (n+)) $ flatten (HM.keys filtered++finished) d graph) filtered
    in HM.unionWith min keys rest

navigate :: ([Char], HM.HashMap Char (HM.HashMap Char ([Char], Int)), Char) -> Int
navigate (finished,graph,current) =
    let Just adjacents = HM.lookup current graph
        filtered = HM.filterWithKey (\k (ds,n) -> k `notElem` finished && all ((`elem` current:finished) . toLower) ds) adjacents
    in if HM.null filtered then 0 else
        minimum $ HM.mapWithKey (\k (ds,n) -> n + navigateMemo (current:finished, graph, k)) filtered

navigateMemo :: ([Char], HM.HashMap Char (HM.HashMap Char ([Char], Int)), Char) -> Int
navigateMemo = memo navigate

mods :: [((Int, Int), Char)]
mods = [((39,39),'1'),((39,40),'#'),((39,41),'2'),((40,39),'#'),((40,40),'#'),((40,41),'#'),((41,39),'3'),((41,40),'#'),((41,41),'4')]

navigateMultiple :: ([Char], HM.HashMap Char (HM.HashMap Char ([Char], Int)), [Char]) -> Int
navigateMultiple (finished,graph,cs) =
    let adjacents = mapMaybe (`HM.lookup` graph) cs
        filterOne = HM.filterWithKey (\k (ds,n) -> k `notElem` finished && all ((\x -> x `elem` cs || x `elem` finished) . toLower) ds)
        filtered = map filterOne adjacents
        minPath r = fmap minimum $ (\x -> if null x then Nothing else Just x) $ HM.elems $ HM.mapWithKey (\k (ds,n) -> n + navigateMultipleMemo ((cs !! r):finished,graph,take r cs ++ [k] ++ drop (r+1) cs)) (filtered !! r)
        ms = mapMaybe minPath [0..3]
    in if null ms then 0 else minimum ms

navigateMultipleMemo :: ([Char], HM.HashMap Char (HM.HashMap Char ([Char], Int)), [Char]) -> Int
navigateMultipleMemo = memo navigateMultiple