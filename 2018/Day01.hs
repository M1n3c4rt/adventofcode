module Day01 where
import qualified Data.Set as S
import Data.List (delete)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/01.txt"
    -- part 1
    print $ sum $ map (read . delete '+') $ lines contents
    -- part 2
    print $ duplicate S.empty $ scanl (+) 0 $ cycle $ map (read . delete '+') $ lines contents

duplicate finished (x:xs)
    | x `S.member` finished = x
    | otherwise = duplicate (S.insert x finished) xs