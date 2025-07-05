module Day2017_14 where
import Day2017_10 (knotHash)
import Numeric (readHex, showBin)
import Utility.AOC (enumerateFilterSet, neighbours4)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Day2017_12 (groups)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/14.txt"
    let g = grid contents
        n = toNetwork g
    -- part 1
    print $ S.size g
    -- part 2
    print $ length $ groups n

toBin :: Char -> [Char]
toBin c = pad $ showBin (fst $ head $ readHex [c]) ""
    where pad x = replicate (4 - length x) '0' ++ x

grid :: String -> S.Set (Int,Int)
grid cs = enumerateFilterSet (=='1') $ unlines $ map (concatMap toBin . knotHash . ((cs++"-")++) . show) [0..127]

toNetwork s = HM.fromList $ S.toList $ S.map helper s
    where
        helper p = (p,filter (`S.member` s) $ neighbours4 p)