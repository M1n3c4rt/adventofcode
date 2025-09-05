module Day2017_24 where
import Data.List.Extra (splitOn, delete, maximumOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/24.txt"
    let bs = allBridges 0 $ map parse $ lines contents
    -- part 1
    print $ maximum $ map (sum . concat) bs
    -- part 2
    print $ sum $ concat $ maximumOn (\b -> (length b, sum $ concat b)) bs

type Component = [Int]
type Bridge = [[Int]]

parse :: String -> [Int]
parse = map read . splitOn "/"

allBridges start remaining
    | null candidates = [[]]
    | otherwise = concatMap (\c -> map (c:) $ allBridges (head $ delete start c) (delete c remaining)) candidates
    where
        candidates = filter (start `elem`) remaining