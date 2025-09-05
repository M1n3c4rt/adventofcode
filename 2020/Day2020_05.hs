module Day2020_05 where

import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/05.txt"
    let passes = map ((\(x,y) -> x*8 + y) . parse) (lines contents)
    -- part 1
    print $ maximum passes
    -- part 2
    print $ findEmpty $ sort passes

parse :: (Num a, Num b) => [Char] -> (a, b)
parse s =
    let (row,col) = splitAt 7 s
        r = sum $ zipWith (*) (map (2^) [6,5..0]) $ map (\c' -> if c' == 'F' then 0 else 1) row
        c = sum $ zipWith (*) (map (2^) [2,1,0]) $ map (\c' -> if c' == 'L' then 0 else 1) col
    in (r,c)

findEmpty :: (Eq a, Num a) => [a] -> a
findEmpty (m:n:ns) = if n - m /= 1 then m+1 else findEmpty (n:ns)