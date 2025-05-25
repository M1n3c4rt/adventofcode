module Day21 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Char (ord, chr)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/21.txt"
    -- part 1
    print $ head $ (filter (>256) . (\x -> run x 0 0 $ parse contents) . map ord . unlines) part1
    -- part 2
    print $ head $ (filter (>256) . (\x -> run x 0 0 $ parse contents) . map ord . unlines) part2

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

run :: [Int] -> Int -> Int -> HM.HashMap Int Int -> [Int]
run inputs pointer relBase state = case inst of
    1 -> run inputs (pointer+4) relBase $ HM.insert cW (a+b) state
    2 -> run inputs (pointer+4) relBase $ HM.insert cW (a*b) state
    3 -> run (tail inputs) (pointer+2) relBase $ HM.insert aW (head inputs) state
    4 -> a : run inputs (pointer+2) relBase state
    5 -> run inputs (if a == 0 then pointer+3 else b) relBase state
    6 -> run inputs (if a /= 0 then pointer+3 else b) relBase state
    7 -> run inputs (pointer+4) relBase $ HM.insert cW (if a < b then 1 else 0) state
    8 -> run inputs (pointer+4) relBase $ HM.insert cW (if a == b then 1 else 0) state
    9 -> run inputs (pointer+2) (relBase+a) state
    99 -> []
    where
        (inst:mods) = splitMode $ HM.lookupDefault 0 pointer state

        vals@(a':b':c':vs) = map (\x -> HM.lookupDefault 0 (pointer+x) state) [1..]
        refs@(a :b :c :rs) = zipWith applyMod vals mods
        refWrites@(aW:bW:cW:rws) = zipWith applyModW vals mods

        applyMod x' m = case m of
            0 -> HM.lookupDefault 0 x' state
            1 -> x'
            2 -> HM.lookupDefault 0 (relBase + x') state

        applyModW x' m = case m of
            0 -> x'
            2 -> relBase + x'

splitMode :: (Integral a, Read a, Show a) => a -> [a]
splitMode n = n `mod` 100 : map (read . pure) (reverse $ show $ n `div` 100) ++ repeat 0

-- ???# : jump
-- ???. : don't jump
--

part1 :: [String]
part1 = ["OR A J","AND B J","AND C J","NOT J J","AND D J","WALK"]
part2 :: [String]
part2 = ["OR A J","AND B J","AND C J","NOT J J","AND D J","OR E T","OR H T","AND T J","RUN"]