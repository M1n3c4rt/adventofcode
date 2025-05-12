module Day07 where

import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.List (nub)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/07.txt"
    let table = HM.fromList $ map parse $ lines contents
    -- part 1
    print $ pred $ length $ nub $ resolveOut ["shiny gold"] table
    -- part 2
    print $ pred $ resolveIn "shiny gold" table

parse :: [Char] -> ([Char], [(Int, [Char])])
parse line =
    let [bag,rest] = splitOn " bags contain " line
        rests = if take 2 rest == "no" then [] else map ((\[a,b,c] -> (read a,b ++ " " ++ c)) . take 3 . words) $ splitOn ", " rest
    in (bag,rests)

resolveOut :: [String] -> HM.HashMap String [(Int, String)] -> [String]
resolveOut cur table
    | null new = cur
    | otherwise = cur ++ resolveOut new table
    where new = HM.keys $ HM.filter (any ((`elem` cur) . snd)) table

resolveIn :: String -> HM.HashMap String [(Int, String)] -> Int
resolveIn cur table = case HM.lookup cur table of
    Just xs -> 1 + sum (map (\(n,b) -> n*resolveIn b table) xs)
    Nothing -> 1