module Day2025_07 where
import Data.List (elemIndices, elemIndex)
import qualified Data.HashMap.Lazy as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/07.txt"
    -- part 1
    let (l:ls) = lines contents
    let Just n = elemIndex 'S' l
    let (a,b) = foldl (\(n',acc) c -> (n'+split' (map fst acc) c,split acc c)) (0,[(n,1)]) ls
    print a
    -- part 2
    print $ sum $ map snd b

split' :: [Int] -> [Char] -> Int
split' ns cs = length $ filter (`elem` elemIndices '^' cs) ns

split :: [(Int, Int)] -> [Char] -> [(Int, Int)]
split ns cs = HM.toList . HM.fromListWith (+) $ concatMap splitBeams ns
    where
        splitBeams (n,k) = if n `elem` elemIndices '^' cs then [(n+1,k),(n-1,k)] else [(n,k)]