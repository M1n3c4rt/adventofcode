import Data.List ( sort, transpose )

main :: IO ()
main = do
    contents <- readFile "01.txt"
    let parsed = transpose $ map getNumbers $ lines contents
    -- part 1
    print $ f parsed
    -- part 2
    print $ g parsed

getNumbers :: String -> [Int]
getNumbers s = map read $ words s :: [Int]

f :: [[Int]] -> Int
f [l1,l2] = sum $ map abs $ zipWith (-) (sort l1) (sort l2)

g :: [[Int]] -> Int
g [l1,l2] = sum $ map (\x -> x * numAppearances x l2) l1

numAppearances :: Int -> [Int] -> Int
numAppearances n l = length [a | a <- l, a == n]