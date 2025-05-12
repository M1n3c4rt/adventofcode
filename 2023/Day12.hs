module Day12 where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.MemoUgly (memo)
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/12.txt"
    -- part 1
    print $ sum $ map findMatchesMemo (parse contents)
    -- part 2
    print $ sum $ map findMatchesMemo (parse' contents)

parse :: String -> [(Int, String, [Int])]
parse = map ((\[a,b] -> (0,a,map read $ splitOn "," b)) . words) . lines

parse' :: String -> [(Int, String, [Int])]
parse' = map ((\[a,b] -> (0,intercalate "?" $ replicate 5 a,concat $ replicate 5 $ map read $ splitOn "," b)) . words) . lines

findMatchesMemo :: (Int, String, [Int]) -> Int
findMatchesMemo = memo findMatches

findMatches :: (Int,String,[Int]) -> Int
findMatches (cur,ss,[]) = if '#' `notElem` ss then 1 else 0
findMatches (cur,[],[n]) = if n == cur then 1 else 0
findMatches (cur,[],ns) = 0
findMatches (cur,s:ss,n:ns)
    | cur > n = 0
    | s == '.' = if cur == n then findMatchesMemo (0,ss,ns) else
        if cur == 0 then findMatchesMemo (0,ss,n:ns) else 0
    | s == '#' = findMatchesMemo (cur+1,ss,n:ns)
    | s == '?' = findMatchesMemo (cur,'.':ss,n:ns) + findMatchesMemo (cur,'#':ss,n:ns)