module Day2024_12 where


main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2024/12.txt"
    let l = readContents 0 $ lines contents
    let regions = concatMap (spl [] . (`getByChar` l)) ['A'..'Z']
    -- part 1
    print $ sum $ map (\x -> length x * perim x) regions
    -- part 2
    print $ sum $ map (\x -> length x * perim' x) regions

spl :: [[(Int,Int)]] -> [(Int,Int)] -> [[(Int,Int)]]
spl ls (p:ps) = let (a,b) = filter2 (any (neighbour p)) ls in spl ((p:concat a):b) ps
spl ls [] = ls

fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b
thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c

filter2 :: (a -> Bool) -> [a] -> ([a], [a])
filter2 f (l:ls)
    | f l = let (a,b) = filter2 f ls in (l:a,b)
    | otherwise = let (a,b) = filter2 f ls in (a,l:b)
filter2 f [] = ([],[])

neighbour :: (Int, Int) -> (Int, Int) -> Bool
neighbour (a,b) (c,d) = abs (c-a) + abs (d-b) == 1

neighbourN :: (Int, Int) -> (Int, Int) -> Bool
neighbourN (a,b) (c,d) = b == d && a-c == 1

neighbourE :: (Int, Int) -> (Int, Int) -> Bool
neighbourE (a,b) (c,d) = a == c && b-d == 1

neighbourW :: (Int, Int) -> (Int, Int) -> Bool
neighbourW (a,b) (c,d) = a == c && b-d == -1

neighbourS :: (Int, Int) -> (Int, Int) -> Bool
neighbourS (a,b) (c,d) = b == d && a-c == -1

perim :: [(Int, Int)] -> Int
perim l = sum $ map (\x -> 4 - length (filter (neighbour x) l)) l

perimN :: [(Int, Int)] -> Int
perimN l = length $ spl [] $ filter (\x -> not $ any (neighbourN x) l) l

perimE :: [(Int, Int)] -> Int
perimE l = length $ spl [] $ filter (\x -> not $ any (neighbourE x) l) l

perimW :: [(Int, Int)] -> Int
perimW l = length $ spl [] $ filter (\x -> not $ any (neighbourW x) l) l

perimS :: [(Int, Int)] -> Int
perimS l = length $ spl [] $ filter (\x -> not $ any (neighbourS x) l) l

perim' :: [(Int, Int)] -> Int
perim' l = perimN l + perimE l + perimW l + perimS l

readContents :: Int -> [String] -> [(Int,Int,Char)]
readContents n (ls:lss) = zip3 (repeat n) [0..] ls ++ readContents (n+1) lss
readContents _ [] = []

getByChar :: Char -> [(Int,Int,Char)] -> [(Int,Int)]
getByChar c l = map (\x -> (fst3 x, snd3 x)) $ filter (\x -> thd3 x == c) l