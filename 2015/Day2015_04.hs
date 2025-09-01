module Day2015_04 where
import Data.Hash.MD5 (md5s, Str (Str))

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/04.txt"
    -- part 1
    print $ head $ filter (all (=='0') . take 5 . md5s . Str . (contents++) . show) [1..]
    -- part 2
    print $ head $ filter (all (=='0') . take 6 . md5s . Str . (contents++) . show) [1..]
