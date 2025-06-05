module Day2018_07 where
import Data.List (sort)
import Data.Char (ord)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/07.txt"
    -- part 1
    putStrLn $ steps (map (\x -> (x!!5,x!!36)) $ lines contents) ['A'..'Z']
    -- part 2
    print $ steps' (map (\x -> (x!!5,x!!36)) $ lines contents) (replicate 5 (' ',-1)) ['A'..'Z']

steps :: [(Char, Char)] -> [Char] -> [Char]
steps table "" = ""
steps table remaining =
    let candidate = minimum $ filter (`notElem` map snd table) remaining
        updated = filter ((/=candidate) . fst) $ filter ((/=candidate) . snd) table
    in candidate : steps updated (filter (/=candidate) remaining)

steps' table workers "" = (+1) $ maximum $ map snd workers
steps' table workers remaining =
    let finished = map fst $ filter ((<0) . snd) workers
        updated = filter ((`notElem` finished) . fst) $ filter ((`notElem` finished) . snd) table
        candidates = sort $ filter (`notElem` map snd updated) remaining

        updateWorkers (c:cs) ((wc,n):ws)
            | wc `elem` finished = (c,ord c - 5 - 1):updateWorkers cs ws
            | otherwise = (wc,n-1):updateWorkers (c:cs) ws
        updateWorkers [] ((wc,n):ws) = ((wc,n-1):updateWorkers [] ws)
        updateWorkers (c:cs) [] = []
        updateWorkers [] [] = []

        newWorkers = updateWorkers candidates workers
    in 1 + steps' updated (updateWorkers candidates workers) (filter (`notElem` map fst newWorkers) remaining)