module Day2019_16 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/16.txt"
    let ns = parse contents
        offset :: Int
        offset = read $ concatMap show $ take 7 ns
        ns' = concat $ replicate 10000 ns
    -- part 1
    putStrLn $ concatMap show $ take 8 $ (!!100) $ iterate (multiplyV (genModMatrix (length ns))) ns
    -- part 2
    putStrLn $ concatMap show $ take 8 $ (!!100) $ iterate nextEntries $ drop offset ns'

parse :: String -> [Int]
parse = map (read . pure)

genModMatrix :: Int -> [[Int]]
genModMatrix n = take n $ map (\x -> take n $ tail $ cycle $ replicate x 0 ++ replicate x 1 ++ replicate x 0 ++ replicate x (-1)) [1..]

multiplyV :: [[Int]] -> [Int] -> [Int]
multiplyV m v = map ((`mod` 10) . abs . sum . zipWith (*) v) m

nextEntries :: [Int] -> [Int]
nextEntries = scanr1 (\acc c -> (c + acc) `mod` 10)