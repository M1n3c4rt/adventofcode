module Day2024_25 where

import Data.List ( transpose )
import Data.List.Split ( splitOn )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2024/25.txt"
    -- part 1
    print $ checkAllPerms $ getAllPerms $ groupByItem ([],[]) $ getItems contents
    -- part 2
    -- gg!

data Item = Lock | Key deriving (Show,Eq)

getItem :: [String] -> (Item,[Int])
getItem s = (if head (head s) == '.' then Key else Lock, map (subtract 1 . length . filter (=='#')) s)

getItems :: String -> [(Item,[Int])]
getItems s = map (getItem . transpose . lines) (splitOn "\n\n" s)

groupByItem :: ([[Int]], [[Int]]) -> [(Item,[Int])] -> ([[Int]], [[Int]])
groupByItem (ls,ks) ((i,l):is) = case i of
    Lock -> groupByItem (l:ls,ks) is
    Key -> groupByItem (ls,l:ks) is
groupByItem (ls,ks) [] = (ls,ks)

getAllPerms :: ([[Int]], [[Int]]) -> [([Int],[Int])]
getAllPerms (a,b) = concatMap (\x -> map (x,) b) a

checkAllPerms :: [([Int],[Int])] -> Int
checkAllPerms ((a,b):xs) = (if all (<6) $ zipWith (+) a b then 1 else 0) + checkAllPerms xs
checkAllPerms [] = 0