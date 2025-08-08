module Day2016_08 where
import Utility.AOC
import Data.List (transpose)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/08.txt"
    -- part 1
    print $ length $ concatMap (filter id) $ foldl (flip parseInst) (replicate 50 (replicate 6 False)) $ lines contents
    -- part 2
    putStrLn $ pprintList $ foldl (flip parseInst) (replicate 50 (replicate 6 False)) $ lines contents

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f 0 (x:xs) = f x : xs
adjust f n (x:xs) = x : adjust f (n-1) xs

parseInst :: String -> ([[Bool]] -> [[Bool]])
parseInst s pxs
    | take 4 s == "rect" =
        let f a' = replicate b True ++ drop b a'
        in map f (take a pxs) ++ drop a pxs
    | take 10 s == "rotate row" =
        let f a' = take (length a') $ drop (length a' - b) $ cycle a'
        in transpose $ adjust f a $ transpose pxs
    | otherwise =
        let f a' = take (length a') $ drop (length a' - b) $ cycle a'
        in adjust f a pxs
    where [a,b] = numbers s

pprintList :: [[Bool]] -> String
pprintList s = prettyPrintSetWide $ S.fromList [(x,6-y) | x <- [0..49], y <- [0..5], s !! x !! y]