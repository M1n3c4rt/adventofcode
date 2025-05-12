module Day13 where


import Data.Either (fromRight)
import Data.List.Split (splitOn)
import Data.Function (on)
import Data.List (sort, findIndices)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/13.txt"
    -- part 1
    print $ sum $ map fst $ filter (\(a,b) -> b == LT) $ zip [1..] $ map ((\[a,b] -> (compare `on` (read::String->Packet)) a b) . lines) $ splitOn "\n\n" contents
    -- part 2
    let dividers = map read ["[[2]]","[[6]]"]
    print $ product $ map (+1) $ findIndices (`elem` dividers) $ sort $ (dividers ++) $ map (read::String->Packet) $ filter (not . null) $ lines contents

newtype Packet = P [Either Int Packet] deriving (Show, Eq)

instance Read Packet where
    readsPrec :: Int -> ReadS Packet
    readsPrec _ s = [(fromRight (P []) $ r' s,"")]
        where r n ds (s:ss)
                | s == '[' = r (n+1) (s:ds) ss
                | s == ']' = r (n-1) (s:ds) ss
                | s == ',' && n == 0 = reverse ds : r n "" ss
                | otherwise = r n (s:ds) ss
              r n ds [] = [reverse ds]
              r' s
                | not (null l) = Right $ P $ map Left $ fst $ head l
                | not (null i) = Left $ fst $ head i
                | otherwise = Right $ P $ map r' . r 0 "" . (tail . init) $ s
                where l = reads s::[([Int],String)]
                      i = reads s::[(Int,String)]

instance Ord Packet where
    compare :: Packet -> Packet -> Ordering
    compare (P a) (P b) = compare a b

instance {-# INCOHERENT #-} Ord (Either Int Packet) where
    compare :: Either Int Packet -> Either Int Packet -> Ordering
    compare (Right (P a)) (Left n) = compare a [Left n]
    compare (Left m) (Right (P b)) = compare [Left m] b
    compare (Right (P a)) (Right (P b)) = compare a b
    compare (Left m) (Left n) = compare m n