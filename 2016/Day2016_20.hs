module Day2016_20 where
import Utility.AOC (numbers')
import Data.List (sortOn)
import Data.Word (Word32)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/20.txt"
    -- part 1
    let gs = gaps $ sortOn fst $ foldr (addToRangePile . (\[a,b] -> (a,b)) . numbers') [] $ lines contents
    print $ head gs
    -- part 2
    print $ length gs

rangeSub :: (Word32, Word32) -> (Word32, Word32) -> [(Word32, Word32)]
rangeSub (x,y) (a,b)
    | x > a && y < b = [(a,x-1),(y+1,b)]
    | x <= a && y >= b = []
    | y >= a && x <= a = [(y+1,b)]
    | x <= b && y >= b = [(a,x-1)]
    | otherwise = [(a,b)]

addToRangePile :: (Word32, Word32) -> [(Word32, Word32)] -> [(Word32, Word32)]
addToRangePile range pile = range : concatMap (rangeSub range) pile

gaps :: [(Word32, Word32)] -> [Word32]
gaps ((a,b):(c,d):rs) = [b+1..c-1] ++ gaps ((c,d):rs)
gaps _ = []