module Day21 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Char (ord, chr)
import Intcode ( parse, runC, getOutput )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/21.txt"
    -- part 1
    print $ head $ (filter (>256) . (\x -> getOutput $ runC x $ parse contents) . map ord . unlines) part1
    -- part 2
    print $ head $ (filter (>256) . (\x -> getOutput $ runC x $ parse contents) . map ord . unlines) part2

part1 :: [String]
part1 = ["OR A J","AND B J","AND C J","NOT J J","AND D J","WALK"]
part2 :: [String]
part2 = ["OR A J","AND B J","AND C J","NOT J J","AND D J","OR E T","OR H T","AND T J","RUN"]