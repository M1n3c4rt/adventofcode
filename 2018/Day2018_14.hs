{-# LANGUAGE BangPatterns #-}
module Day2018_14 where
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/14.txt"
    let n = read contents
    -- part 1
    putStrLn $ intercalate "" $ map show $ (\x -> mapMaybe (`HM.lookup` x) [n..n+9]) $ genSeq (n+10) 0 1 2 $ HM.fromList [(0,3),(1,7)]
    -- part 2
    print $ searchSeq (map (read . pure) contents) [0,0,0,0,0,3,7] 0 1 2 $ HM.fromList [(0,3),(1,7)]

genSeq :: Int -> Int -> Int -> Int -> HM.HashMap Int Int -> HM.HashMap Int Int
genSeq limit a b n !finished
    | n == limit = finished
    | a'+ b' < 10 = genSeq limit ((a+a'+1) `mod` (n+1)) ((b+b'+1) `mod` (n+1)) (n+1) $ HM.insert n (a'+b') finished
    | otherwise = genSeq limit ((a+a'+1) `mod` (n+2)) ((b+b'+1) `mod` (n+2)) (n+2) $ HM.insert (n+1) ((a'+b') `mod` 10) $ HM.insert n ((a'+b') `div` 10) finished
    where
        a' = HM.lookupDefault 0 a finished
        b' = HM.lookupDefault 0 b finished

searchSeq :: [Int] -> [Int] -> Int -> Int -> Int -> HM.HashMap Int Int -> Int
searchSeq query window a b n !finished
    | query == init window = n-length query-1
    | query == tail window = n-length query
    | a'+ b' < 10 = searchSeq query (tail window ++ [a'+b']) ((a+a'+1) `mod` (n+1)) ((b+b'+1) `mod` (n+1)) (n+1) $ HM.insert n (a'+b') finished
    | otherwise = searchSeq query (drop 2 window ++ [(a'+b')`div`10,(a'+b')`mod`10]) ((a+a'+1) `mod` (n+2)) ((b+b'+1) `mod` (n+2)) (n+2) $ HM.insert (n+1) ((a'+b') `mod` 10) $ HM.insert n ((a'+b') `div` 10) finished
    where
        a' = HM.lookupDefault 0 a finished
        b' = HM.lookupDefault 0 b finished

toList' n m = mapMaybe (`HM.lookup` m) [0..n-1]
findSeq n s ns@(r:rest)
    | and (zipWith (==) s ns) = n
    | otherwise = findSeq (n+1) s rest