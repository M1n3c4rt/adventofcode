module Day2019_08 where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (transpose, intersperse)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/08.txt"
    -- part 1
    print $ (\x -> length (filter (==1) x) * length (filter (==2) x)) $
        minimumBy (on compare $ length . filter (==0)) $ getLayers contents
    -- part 2
    putStr $ pprint $ getPixels $ transpose $ getLayers contents

getLayers :: String -> [[Int]]
getLayers = map (map (read . pure)) . repeatTake 150

repeatTake :: Int -> [a] -> [[a]]
repeatTake n [] = []
repeatTake n ls = take n ls:repeatTake n (drop n ls)

getPixels :: [[Int]] -> [Int]
getPixels = map (head . dropWhile (== 2))

pprint :: [Int] -> String
pprint =
    unlines . map (concatMap (\x -> if x == '1' then "##" else "  ") . concatMap show) . repeatTake 25