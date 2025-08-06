module Day2016_02 where

import Utility.AOC
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust, fromMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/02.txt"
    -- part 1
    putStrLn $ tail $ scanl (\acc c -> foldl move acc $ map toDir c) '5' $ lines contents
    -- part 2
    putStrLn $ tail $ scanl (\acc c -> foldl move' acc $ map toDir c) '5' $ lines contents

toDir :: Char -> ComplexL1 Int
toDir c = case c of
    'R' -> 1 :+ 0
    'L' -> (-1) :+ 0
    'U' -> 0 :+ (-1)
    'D' -> 0 :+ 1

move :: Char -> ComplexL1 Int -> Char
move current d = fromMaybe current $ (`HM.lookup` m') $ (+d) $ fromJust $ HM.lookup current m
    where
        l = enumerate' "123\n456\n789"
        m = HM.fromList $ map (\(a,b) -> (b,uncurry (:+) a)) l
        m' = HM.mapKeys (uncurry (:+)) $ HM.fromList l

move' :: Char -> ComplexL1 Int -> Char
move' current d = fromMaybe current $ (`HM.lookup` m') $ (+d) $ fromJust $ HM.lookup current m
    where
        l = enumerate' "  1  \n 234 \n56789\n ABC \n  D  "
        m = HM.fromList $ filter ((/=' ') . fst) $ map (\(a,b) -> (b,uncurry (:+) a)) l
        m' = HM.filter (/=' ') $ HM.mapKeys (uncurry (:+)) $ HM.fromList l