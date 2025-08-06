module Day2016_04 where
import Data.Char (isDigit, isAlpha, chr, ord)
import Data.List.Extra (splitOn, sortOn, sort, group)
import Data.Ord (Down(Down))

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/04.txt"
    -- part 1
    print $ sum $ map (isReal . parseLine) $ lines contents
    -- part 2
    print $ (\[(a,b,c)] -> b) $ filter ((=="northpole object storage") . decrypt) $ map parseLine $ lines contents

parseLine :: [Char] -> ([[Char]], Int, [Char])
parseLine s = let ls = splitOn "-" s in (init ls,read $ filter isDigit $ last ls, filter isAlpha $ last ls)

isReal :: ([[Char]], Int, [Char]) -> Int
isReal (a,n,c) = if (c==) $ take 5 $ map head $ sortOn (\ls -> (-length ls, head ls)) $ group $ sort $ concat a then n else 0

decrypt :: ([[Char]], Int, [Char]) -> [Char]
decrypt (a,n,_) = unwords $ map (map shift) a
    where shift c = chr $ ((ord c - 97 + n) `mod` 26) + 97