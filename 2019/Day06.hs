module Day06 where

import Data.List.Split (splitOn)
import Data.Either (rights)
import Data.Maybe (listToMaybe, mapMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/06.txt"
    let tree = foldInsert [] (Node "COM" []) $ parse contents
    -- part 1
    print $ getAllOrbits tree
    -- part 2
    print $ distance "YOU" "SAN" tree

data Tree = Node String [Tree] deriving (Show, Eq)

parse :: String -> [(String,String)]
parse = map (\x -> let a:b:_ = splitOn ")" x in (a,b)) . lines

member :: String -> Tree -> Bool
member x (Node y ys) = y == x || any (member x) ys

insert :: (String, String) -> Tree -> Either Tree Tree
insert (a,b) (Node y ys)
    | y == a = Right (Node y (Node b []:ys))
    | null ys = Left (Node y ys)
    | otherwise =
        case rights $ map (insert (a,b)) ys of
            [] -> Left (Node y ys)
            t@(Node x xs):ts -> Right (Node y (t:filter (\(Node z zs) -> z /= x) ys))

foldInsert :: [(String, String)] -> Tree -> [(String, String)] -> Tree
foldInsert [] tree [] = tree
foldInsert unfinished tree [] = foldInsert [] tree unfinished
foldInsert unfinished tree (i:items) = case insert i tree of
    Right tree' -> foldInsert unfinished tree' items
    Left _ -> foldInsert (i:unfinished) tree items

fix :: Tree -> Tree
fix t'@(Node y []) = t'
fix t@(Node y ys) =
    Node y $ filter (\(Node y' _) -> length (filter (member y') ys) == 1) $ map fix ys

getAllOrbits :: Tree -> Int
getAllOrbits (Node y []) = 0
getAllOrbits (Node y ys) = sum (map size ys) + sum (map getAllOrbits ys)
    where
        size (Node y []) = 1
        size (Node y ys) = 1 + sum (map size ys)

distance :: String -> String -> Tree -> Int
distance a b tree@(Node y ys) = case filter (\t -> member a t && member b t) ys of
            [] -> depth a tree + depth b tree
            xs -> head $ map (distance a b) xs
    where
        depth z (Node t ts)
            | t == z = -1
            | otherwise = (1+) $ depth z $ head $ filter (member z) ts