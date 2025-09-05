module Day2017_10 where
import Data.Bifunctor (first)
import Data.List.Split (splitOn)
import Data.Char (ord)
import Utility.AOC (chunk)
import Data.Bits (Bits(xor))
import Numeric (showHex)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/10.txt"
    -- part 1
    print $ product $ map fst $ take 2 $ dropWhile ((/=0) . snd) $ knotHash' contents
    -- part 2
    putStrLn $ knotHash contents

knot ls (n,skip) = take (length ls) $ drop (n+skip) $ cycle $ uncurry zip (first reverse $ unzip a) ++ b
    where
        (a,b) = splitAt n ls

knotHash' s = foldl knot (zip [0..255] [0..255]) $ zip (map read $ splitOn "," s) [0..]

knotHash :: String -> String
knotHash s = cs
    where
        base = take 256 $ dropWhile ((/=0) . snd) $ cycle $ foldl knot (zip [0..255] [0..255]) $ zip (concat $ replicate 64 $ map ord s ++ [17,31,73,47,23]) [0..]
        cs = concatMap (toHex . foldr1 xor) (chunk 16 $ map fst base :: [[Int]])
        toHex n = let s' = showHex n "" in if length s' == 1 then '0':s' else s'