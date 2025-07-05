module Day2017_15 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/15.txt"
    let [a,b] = map (read . drop 24) $ lines contents
    -- part 1
    print $ score (a,b)
    -- part 2
    print $ score' (a,b)

score (a,b) = length $ filter id $ zipWith (==) (it 16807 a) (it 48271 b)
    where
        it factor = take 40000000 . map (`mod` 65536) . iterate ((`mod` (2^31-1)) . (*factor))

score' (a,b) = length $ filter id $ zipWith (==) (it 16807 4 a) (it 48271 8 b)
    where
        it factor m = take 5000000 . filter ((==0) . (`mod` m)) . map (`mod` 65536) . iterate ((`mod` (2^31-1)) . (*factor))