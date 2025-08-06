module Day2016_01 where
import Data.List.Extra (splitOn, nubOrd)
import Utility.AOC (ComplexL1 (..), magnitude)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/01.txt"
    -- part 1
    print $ magnitude $ snd $ foldl (flip ($)) (0:+1,0:+0) $ map toDir $ splitOn ", " contents
    -- part 2
    print $ magnitude $ fst $ head $ dropWhile (uncurry (==)) $ (\l -> zip l $ nubOrd l) $ concatMap (uncurry between) $ (\l -> zip l $ tail l) $ map snd $ scanl (flip ($)) (0:+1,0:+0) $ map toDir $ splitOn ", " contents

toDir :: String -> (ComplexL1 Int,ComplexL1 Int) -> (ComplexL1 Int,ComplexL1 Int)
toDir (s:n) (dir,z) = case s of
    'R' -> let dir' = dir*(0:+(-1)) in (dir',dir'*fromInteger (read n)+z)
    'L' -> let dir' = dir*(0:+1) in (dir',dir'*fromInteger (read n)+z)

between :: ComplexL1 Int -> ComplexL1 Int -> [ComplexL1 Int]
between z1 z2 = let c = signum (z2-z1) in takeWhile (/=z2) $ iterate (+c) z1