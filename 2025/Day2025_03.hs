module Day2025_03 where
import Data.List (sortOn)
import Data.Char (ord)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/03.txt"
    -- part 1
    print $ sum $ map (read . keep 2) $ lines contents
    -- part 2
    print $ sum $ map (read . keep 12) $ lines contents

keep :: Int -> [Char] -> [Char]
keep 0 _ = ""
keep n s = if n == length s then s else let (c:cs) = drop maxIndex s in c : keep (n-1) cs
    where maxIndex = head $ filter (\i -> length s - i >= n) $ map snd $ sortOn (\(c,i) -> (-ord c,i)) (zip s [0..])