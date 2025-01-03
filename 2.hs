import Data.List ( sort )

main :: IO ()
main = do
    contents <- readFile "2.txt"
    -- part 1
    print $ foldl (flip ((+) . fromEnum)) 0 $ map (getSafe . getNumbers) (lines contents)
    -- part 2
    print $ foldl (flip ((+) . fromEnum)) 0 $ map (damp . getNumbers) (lines contents)
    
getNumbers :: String -> [Int]
getNumbers s = map read $ words s :: [Int]

damp :: [Int] -> Bool
damp l = getSafe l || any getSafe (genLists l)

getSafe :: [Int] -> Bool
getSafe l = checkOrder l && all (\x -> x /= 0 && x < 4) (getDiff l)

checkOrder :: [Int] -> Bool
checkOrder l = (l == sort l) || (reverse l == sort l)

getDiff :: [Int] -> [Int]
getDiff l = tail $ map abs $ zipWith (-) l $ head l:l

genLists :: [Int] -> [[Int]]
genLists [] = []
genLists (x:xs) = xs:map (x:) (genLists xs)