module Day2017_20 where
import Utility.AOC (numbers, taxicab)
import Data.Bifunctor (second)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (group, sort)
import Data.List.Extra (groupSortOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/20.txt"
    -- part 1
    print $ (!!1000) $ map closest $ iterate step $ zip [0..] $ map parse $ lines contents
    -- part 2
    print $ (!!1000) $ map length $ iterate (destroy . step) $ zip [0..] $ map parse $ lines contents

parse :: String -> [(Int,Int,Int)]
parse = (\[a,b,c,d,e,f,g,h,i] -> [(a,d,g),(b,e,h),(c,f,i)]) . numbers

update :: (Int, Int, Int) -> (Int, Int, Int)
update (s,v,a) = (s+v+a,v+a,a)

step :: [(Int, [(Int, Int, Int)])] -> [(Int, [(Int, Int, Int)])]
step = map (second (map update))

closest :: [(Int, [(Int,Int,Int)])] -> Int
closest = fst . minimumBy (compare `on` ((taxicab [0,0,0] . map (\(a,b,c) -> a)) . snd))

destroy :: [(Int, [(Int,Int,Int)])] -> [(Int, [(Int,Int,Int)])]
destroy = map head . filter ((==1) . length) . groupSortOn (map (\(a,b,c) -> a) . snd)