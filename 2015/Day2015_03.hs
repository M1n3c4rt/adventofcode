module Day2015_03 where
import Data.List (nub)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/03.txt"
    -- part 1
    print $ length $ nub $ scanl (\acc c -> tupAdd acc $ toDir c) (0,0) contents
    -- part 2
    print $ length $ nub $ concatMap (scanl (\acc c -> tupAdd acc $ toDir c) (0,0)) $ tear contents

toDir '>' = ( 1, 0)
toDir '<' = (-1, 0)
toDir 'v' = ( 0, 1)
toDir '^' = ( 0,-1)
toDir _ = (0,0)

tupAdd (a,b) (c,d) = (a+c,b+d)

tear (a:b:bs) = let [x,y] = tear bs in [a:x,b:y]
tear [] = [[],[]]