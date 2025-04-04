import Utility.AOC (choose)
import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "09.txt"
    let part1 = search $ initialize $ map read $ lines contents :: Int
    -- part 1
    print part1
    -- part 2
    print $ findRangeSum part1 $ map read $ lines contents

initialize :: [a] -> ([[(a, a)]], [a])
initialize ns = (choose2' (take 25 ns), drop 25 ns)

search :: (Eq t, Num t, Show t) => ([[(t, t)]], [t]) -> t
search (chooses,n:ns)
    | n `elem` concatMap (map (uncurry (+))) chooses = search ((++[[]]) $ map extrude $ tail chooses,ns)
    | otherwise = n
    where
        extrude [] = [(l,n)]
        extrude ns'@((a,b):rest) = ns' ++ [(a,n)]
        l = snd $ last $ head chooses

choose2' :: [a] -> [[(a, a)]]
choose2' (n:ns) = map (n,) ns : choose2' ns
choose2' [] = []

findRangeSum :: (Ord t, Num t, Show t) => t -> [t] -> t
findRangeSum k ns@(n:rest)
    | last candidate == k = let range = take (length candidate - 1) ns in maximum range + minimum range
    | otherwise = findRangeSum k rest
    where candidate = takeWhile (<= k) $ scanl (+) 0 ns