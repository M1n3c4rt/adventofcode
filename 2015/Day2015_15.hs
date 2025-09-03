module Day2015_15 where
import Data.List (transpose)
import Utility.AOC (numbers)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/15.txt"
    -- part 1
    print $ maximum $ map (`scoreFromCombo` parse contents) $ combo 4 100
    -- part 2
    print $ maximum $ map (`scoreFromCombo'` parse contents) $ combo 4 100

combo 1 n = [[n]]
combo k n = concatMap (\n' -> map (n':) $ combo (k-1) (n-n')) [0..n]

scoreFromCombo ns stats = product . map (max 0 . sum) $ transpose $ zipWith (\k ls -> map (k*) $ init ls) ns stats
scoreFromCombo' ns stats = product . (\l -> if last l == 500 then init l else [0]) . map (max 0 . sum) $ transpose $ zipWith (\k ls -> map (k*) ls) ns stats

parse = map numbers . lines