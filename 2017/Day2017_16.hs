{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Day2017_16 where
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Utility.AOC (extrapolate)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/16.txt"
    -- part 1
    print $ dance ['a'..'p'] $ splitOn "," contents
    -- part 2
    print $ extrapolate 1000000000 $ iterate (`dance` splitOn "," contents) ['a'..'p']

data S = A S | B S

spin :: Int -> [a] -> [a]
spin n = take 16 . drop (16-n) . cycle

exchange :: Int -> Int -> [b2] -> [b2]
exchange a b = map snd . sortOn fst . zip ([0..a-1]++[b]++[a+1..b-1]++[a]++[b+1..15])

partner a b (x:xs)
    | x == a = b:partner a b xs
    | x == b = a:partner a b xs
    | otherwise = x:partner a b xs
partner a b [] = []

exInst :: [Char] -> String -> [Char]
exInst xs (s:ss) = case s of
    's' -> spin (read ss) xs
    'x' -> let [a,b] = map read $ splitOn "/" ss in exchange (min a b) (max a b) xs
    'p' -> partner (head ss) (last ss) xs

dance :: [Char] -> [String] -> [Char]
dance = foldl exInst