import Data.List (sort, group)
import Data.MemoUgly (memo)

main :: IO ()
main = do
    contents <- readFile "10.txt"
    -- part 1
    print $ (\(x,y) -> length x * (length y + 1)) $ span (==1) $ sort $ diffList $ (0:) $ sort $ map read $ lines contents
    -- part 2
    print $ combo $ diffList $ (0:) $ sort $ map read $ lines contents

diffList :: Num a => [a] -> [a]
diffList (x:y:ys) = (y-x):diffList (y:ys)
diffList _ = []

combo :: [Integer] -> Int
combo = product . map helper . group
    where
        helper xs = if head xs == 3 then 1 else case length xs of
            1 -> 1
            2 -> 2
            3 -> 4
            4 -> 7