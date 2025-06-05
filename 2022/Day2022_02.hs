module Day2022_02 where

import Prelude hiding (read)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/02.txt"
    -- part 1
    print $ sum $ map score $ parse contents
    -- part 2
    print $ sum $ map score' $ parse contents

data Play = R | P | S deriving (Show, Eq, Enum)

read :: Char -> Play
read 'A' = R
read 'B' = P
read 'C' = S
read 'X' = R
read 'Y' = P
read 'Z' = S

parse :: String -> [(Play,Play)]
parse = map (\[a,b,c] -> (read a, read c)) . lines

instance Ord Play where
    (<=) :: Play -> Play -> Bool
    R <= P = True
    P <= S = True
    S <= R = True
    x <= y = x == y

score :: (Play,Play) -> Int
score (a,b)
    | a > b = 0 + e
    | a == b = 3 + e
    | a < b = 6 + e
    where e = 1 + fromEnum b

score' :: (Play, Play) -> Int
score' (a,b) = case b of
    R -> 0 + ((e-1) `mod` 3) + 1
    P -> 3 + ((e+0) `mod` 3) + 1
    S -> 6 + ((e+1) `mod` 3) + 1
    where e = fromEnum a