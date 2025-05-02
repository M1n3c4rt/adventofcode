import Data.List.Split ( splitOn )
import Data.Function (on)
import Data.List (groupBy, transpose, sortOn)
import Data.Char (isDigit)
import Debug.Trace (traceShowId, traceShow)

main :: IO ()
main = do
    contents <- readFile "16.txt"
    let (ranges,ticket,tickets) = parse contents
    -- part 1
    print $ sum $ concatMap (filter (\n -> not (any (n `inRange`) ranges))) tickets
    let filteredTickets = transpose $ filter (all (\n -> any (n `inRange`) ranges)) tickets
    -- part 2
    print $ product $ map (ticket !!) $ take 6 $ map fst $ sortOn snd $ solveRanges filteredTickets ranges [] [] $ map (,[0..length ranges-1]) [0..length filteredTickets - 1]

parse :: String -> ([((Int,Int),(Int,Int))],[Int],[[Int]])
parse contents =
    let [a,b,c] = splitOn "\n\n" contents
        ranges = map ((\[c1,c2,c3,c4,c5,c6,c7,c8] -> ((read c2,read c4),(read c6,read c8))) . groupBy ((==) `on` isDigit)) $ lines a
        ticket = map read . splitOn "," $ (!!1) $ lines b
        tickets = map (map read . splitOn ",") $ tail $ lines c
    in (ranges,ticket,tickets)

inRange :: Int -> ((Int, Int), (Int, Int)) -> Bool
inRange n ((a,b),(c,d)) = (n >= a && n <= b) || (n >= c && n <= d)

solveRanges :: [[Int]] -> [((Int, Int), (Int, Int))] -> [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])] -> [(Int, Int)]
solveRanges tickets ranges solved [] [] = solved
solveRanges tickets ranges solved unsolved [] = solveRanges tickets ranges solved [] $ flatten unsolved
    where
        flatten xs = map subflatten xs
            where
                subflatten (n,ys) = let candidates = filter (\y -> length (filter (\x -> y `elem` snd x) (xs ++ map (\(a,b) -> (a,[b])) solved)) == 1) ys
                    in if not (null candidates) then (n,candidates) else (n,ys)

solveRanges tickets ranges solved unsolved ((n,rs):rangeMap)
    | length thinned == 1 = solveRanges tickets ranges ((n,head thinned):solved) unsolved rangeMap
    | otherwise = solveRanges tickets ranges solved ((n,thinned):unsolved) rangeMap
    where thinned = filter (\r -> all (`inRange` (ranges !! r)) $ tickets !! n) rs