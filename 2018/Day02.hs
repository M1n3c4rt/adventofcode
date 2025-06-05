module Day02 where
import Data.List (group, sort)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/02.txt"
    -- part 1
    let nums = map (map length . group . sort) $ lines contents
    print $ length (filter (2 `elem`) nums) * length (filter (3 `elem`) nums)
    -- part 2
    putStrLn $ uncurry split $ head $ filter (uncurry validate) $ choose $ lines contents

validate :: String -> String -> Bool
validate (x:xs) (y:ys)
    | x /= y = xs == ys
    | x == y = validate xs ys
validate [] [] = False

choose (x:xs) = map (x,) xs ++ choose xs
choose [] = []

split (x:xs) (y:ys)
    | x /= y = split xs ys
    | x == y = x : split xs ys
split [] [] = []