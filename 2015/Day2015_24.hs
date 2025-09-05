module Day2015_24 where
import Data.List (sortOn)
import Data.Ord (Down(Down))
import qualified Data.Set as S
import Data.List.Extra (groupOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/24.txt"
    -- part 1
    let containers = sortOn Down $ map read $ lines contents
    print $ minimum $ map product $ head $ groupOn S.size $ sortOn S.size $ combos containers (sum containers `div` 3)
    -- part 2
    print $ minimum $ map product $ head $ groupOn S.size $ sortOn S.size $ combos containers (sum containers `div` 4)

combos :: [Int] -> Int -> [S.Set Int]
combos containers 0 = [S.empty]
combos containers n = let as = init $ scanr (:) [] $ dropWhile (>n) $ containers in
        concatMap (\(n':ns) -> map (S.insert n') $ combos ns (n-n')) as

split :: [Int] -> Int -> Int -> [[S.Set Int]]
split containers 1 n = [[S.fromList containers] | sum containers == n]
split (c:containers) k n =
    let firsts = map (S.insert c) $ combos containers (n-c)
    in concatMap (\f -> map (f:) $ split (filter (`S.notMember` f) containers) (k-1) n) firsts