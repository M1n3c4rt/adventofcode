
main :: IO ()
main = do
    contents <- readFile "9.txt"
    let lists = parse contents
    -- part 1
    print $ sum $ map (extrapolateWith sum last) lists
    -- part 2
    print $ sum $ map (extrapolateWith (foldr (-) 0) head) lists

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diffList :: Num a => [a] -> [a]
diffList [x] = []
diffList (x:y:ys) = y-x:diffList (y:ys)

allDiffLists :: (Eq a, Num a) => [a] -> [[a]]
allDiffLists ls
    | all (==0) ls = []
    | otherwise = let next = diffList ls in next:allDiffLists next

extrapolateWith :: (Eq a, Num a) => ([b] -> c) -> ([a] -> b) -> [a] -> c
extrapolateWith f g ls = f . map g $ (ls:allDiffLists ls)