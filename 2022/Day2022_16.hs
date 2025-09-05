module Day2022_16 where

import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import qualified Data.Bifunctor
import Data.List (sortOn)
import Utility.AOC (shortestDistance)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/16.txt"
    
    let tunnels = parse contents
    let important = filterImportant tunnels
    let valves = flattenGraph important $ getGraph tunnels
    let valvesP = map (Data.Bifunctor.bimap HM.fromList HM.fromList) $ choose' 7 $ HM.toList valves
    -- part 1
    print $ move valves 30 "AA"
    -- part 2
    let candidates = map (\(a,b) -> move a 26 "AA" + move b 26 "AA") valvesP
    print $ maximum candidates

type Tunnels = HM.HashMap String (Int,[String])
type Valves = HM.HashMap String (Int,[(String,Int)])

parse :: String -> Tunnels
parse = HM.fromList . map helper . lines
    where helper l =
            let [a,b] = splitOn ";" l
                a' = take 2 $ drop 6 a
                b' = read $ drop 23 a
                c' = map (take 2) $ drop 4 $ words b
            in (a',(b',c'))

move :: Valves -> Int -> String -> Int
move valves 0 current = 0
move valves timeleft current
    | current == "AA" = unopened
    | n == 0 = 0
    | otherwise = opened
    where
        (n,ns') = HM.lookupDefault (0,[]) current valves
        ns = sortOn snd ns'
        unopened = maximum' $ map (\(s,k) -> move valves (timeleft-k) s) $ filter ((<=timeleft) . snd) ns
        opened = (+(n*(timeleft-1))) $ maximum' $ map (\(s,k) -> move (HM.adjust (\(_,ns'') -> (0,ns'')) current valves) (timeleft-k-1) s) $ filter ((<=timeleft-1) . snd) ns

filterImportant :: Tunnels -> [(String, Int)]
filterImportant = HM.toList . HM.map fst . HM.filterWithKey (\k v -> fst v > 0 || k == "AA")

getGraph :: Tunnels -> Valves
getGraph = HM.map (Data.Bifunctor.second (map (,1)))

flattenGraph :: [(String, Int)] -> Valves -> Valves
flattenGraph important valves =
    let valves' = HM.map snd valves
        importants = genLists important
        getNode ((n,k),ns) = (n,(k,) $ map (\m -> (m, fromJust $ shortestDistance valves' n m)) ns)
    in HM.fromList $ map getNode importants

genLists :: [(String, Int)] -> [((String, Int),[String])]
genLists [] = []
genLists (x:xs) = (x,map fst xs):map (Data.Bifunctor.second (fst x:)) (genLists xs)

maximum' :: [Int] -> Int
maximum' xs = if null xs then 0 else maximum xs

choose :: Int -> [(String, b)] -> [[(String, b)]]
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ if fst x == "AA" then [] else choose n xs
choose 0 xs = [[]]
choose n [] = []

choose' :: Eq b => Int -> [(String, b)] -> [([(String, b)], [(String, b)])]
choose' n xs = map (\c -> (c,filter (\x -> x `notElem` c || fst x == "AA") xs)) $ choose n xs