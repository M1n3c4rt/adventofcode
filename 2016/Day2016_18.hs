module Day2016_18 where
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/18.txt"
    -- part 1
    print $ length $ concatMap (filter not) $ take 40 $ iterate stepL (map (=='^') contents)
    -- part 2
    print $ length $ concatMap (filter not) $ take 400000 $ iterate stepL (map (=='^') contents)

step :: Bool -> Bool -> Bool -> Bool
step a b c = (a == b && b /= c) || (a /= b && b == c)

stepL :: [Bool] -> [Bool]
stepL l = zipWith3 step (False:init l) l (tail l++[False])