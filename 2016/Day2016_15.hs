module Day2016_15 where
import Utility.AOC (numbers)
import Data.List (sortOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/15.txt"
    -- part 1
    print $ crt (1,1) $ sortOn snd $ map ((\[a,b,c,d] -> ((-d-a)`mod`b,b)) . numbers) $ lines contents
    -- part 2
    let new = "Disc #7 has 11 positions; at time=0, it is at position 0."
    print $ crt (1,1) $ sortOn snd $ map ((\[a,b,c,d] -> ((-d-a)`mod`b,b)) . numbers) $ new : lines contents

crt :: (Int,Int) -> [(Int,Int)] -> Int
crt (c,prod) ((r,n):ms) =
    if c `mod` n == r then
        if null ms then c else crt (c,prod*n) ms
    else crt (c+prod,prod) ((r,n):ms)