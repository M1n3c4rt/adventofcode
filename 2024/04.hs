import Data.List ( sort, transpose )

main :: IO ()
main = do
    contents <- readFile "04.txt"
    -- part 1
    print $ fullSearch $ lines contents
    -- part 2
    print $ fullSearch' $ lines contents
    
searchHorizontal :: String -> Int
searchHorizontal s
    | length s < 4 = 0
    | take 4 s == "XMAS" = 1 + searchHorizontal (tail s)
    | otherwise = searchHorizontal $ tail s

diagTranspose :: Int -> [[Char]] -> [[Char]]
diagTranspose n ls
    | not $ all null (safetail (take n ls)) = safehead (take n ls):diagTranspose (min (n+1) $ length ls) (safetail (take n ls) ++ drop n ls)
    | otherwise = [safehead (take n ls)]

safehead :: [[Char]] -> [Char]
safehead (l:ls)
    | null l = safehead ls
    | null ls = [head l]
    | otherwise = head l:safehead ls

safetail :: [[Char]] -> [[Char]]
safetail (l:ls)
    | null l = safetail ls
    | null ls = [tail l]
    | otherwise = tail l:safetail ls

mr :: [[Char]] -> [[Char]]
mr = map reverse
d :: [[Char]] -> [[Char]]
d = diagTranspose 1
t :: [[Char]] -> [[Char]]
t = transpose
ms :: [[Char]] -> [Int]
ms = map searchHorizontal

fullSearch :: [[Char]] -> Int
fullSearch l = sum $ concatMap ms [l,t l, d l, mr l, d $ mr l, mr $ d l, mr $ t l, mr $ d $ mr l]

fullSearch' :: [String] -> Int
fullSearch' (j:k:l:ls) = searchThree j k l + fullSearch' (k:l:ls)
fullSearch' (k:l:ls) = 0
fullSearch' (l:ls) = 0
fullSearch' [] = 0

searchThree :: String -> String -> String -> Int
searchThree (j1:j2:j3:js) (k1:k2:k3:ks) (l1:l2:l3:ls)
    | k2 `elem` "A" && sort [j1,j3,l1,l3] == "MMSS" && j1 /= l3 = 1 + searchThree (j2:j3:js) (k2:k3:ks) (l2:l3:ls)
    | otherwise = searchThree (j2:j3:js) (k2:k3:ks) (l2:l3:ls)
searchThree (j1:j2:js) (k1:k2:ks) (l1:l2:ls) = 0
searchThree (j1:js) (k1:ks) (l1:ls) = 0
searchThree [] [] [] = 0