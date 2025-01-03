import Data.Char (digitToInt)
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- readFile "9.txt"
    let (x,y) = (getZipsA contents, getZipsB contents)
    -- part 1
    print $ foldS 0 (length x - 1) x y (length x) (length y) 0 (last x)
    -- part 2
    print $ checkSum 0 $ foldS' (zip3 [0..] (reverse x) (y++[0])) $ reverse (zip3 [0..] (reverse x) (y++[0]))
    
f :: Int -> Int -> Int -> Int
f a b 0 = 0
f a b c = a*b + f a (b + 1) (c - 1)

everyOtherElement :: [a] -> [a]
everyOtherElement (k:l:ls) = k:everyOtherElement ls
everyOtherElement [l] = [l]
everyOtherElement [] = []

getZipsA :: String -> [Int]
getZipsA s = reverse $ map digitToInt $ everyOtherElement s
getZipsB :: String -> [Int]
getZipsB s = map digitToInt $ everyOtherElement $ tail s

takeLast :: Int -> [a] -> a
takeLast n l = l !! max 0 (n-1)

sumBack :: Int -> [Int] -> Int
sumBack 0 l = 0
sumBack n [] = 0
sumBack n l = last l + sumBack (n-1) (init l)

fst3 :: (Int, Int, Int) -> Int
fst3 (a,b,c) = a
snd3 :: (Int, Int, Int) -> Int
snd3 (a,b,c) = b
thd3 :: (Int, Int, Int) -> Int
thd3 (a,b,c) = c

foldS :: Int -> Int -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int
foldS a b (l1:l1s) (l2:l2s) p q m n
    | q == 1 = f a m (takeLast p (l1:l1s)) + f b n l1
    | l1 == l2 = f a m (takeLast p (l1:l1s)) + f b n l1 + foldS (a+1) (b-1) l1s l2s (p-2) (q-2) (n+l2) (n+l2+takeLast (p-1) (l1:l1s))
    | l1 < l2 = f b n l1 + foldS a (b-1) l1s ((l2-l1):l2s) (p-1) (q-1) m (n+l1)
    | l1 > l2 = f a m (takeLast p (l1:l1s)) + f b n l2 + foldS (a+1) b ((l1-l2):l1s) l2s (p-1) (q-1) (n+l2) (n+l2+takeLast (p-1) (l1:l1s))

foldS' :: [(Int,Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
foldS' l x
    | null x = l
    | all (\y -> thd3 y < snd3 (head x)) (takeWhile (\y -> fst3 y /= fst3 (head x)) l) = foldS' l (tail x)
    | otherwise = foldS' (add (head x) b) (tail x)
    where (a,b) = remove (fst3 $ head x) l

foldSD' :: [(Int,Int,Int)] -> [(Int,Int,Int)] -> [[(Int,Int,Int)]]
foldSD' l x
    | null x = [l]
    | all (\y -> thd3 y < snd3 (head x)) (takeWhile (\y -> fst3 y /= fst3 (head x)) l) = l:foldSD' l (tail x)
    | otherwise = l:foldSD' (add (head x) b) (tail x)
    where (a,b) = remove (fst3 $ head x) l

remove :: Int -> [(Int, Int, Int)] -> ((Int,Int,Int),[(Int, Int, Int)])
remove n (x:y:ys)
    | fst3 y == n = (y,(fst3 x, snd3 x, thd3 x + snd3 y + thd3 y):ys)
    | otherwise = let (a,b) = remove n (y:ys) in (a,x:b)

add :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
add p (q:qs)
    | thd3 q >= snd3 p = [(fst3 q, snd3 q, 0),(fst3 p, snd3 p, thd3 q - snd3 p)] ++ qs 
    | otherwise = q:add p qs

checkSum :: Int -> [(Int, Int, Int)] -> Int
checkSum n (l:ls) = f (fst3 l) n (snd3 l) + checkSum (n+snd3 l+thd3 l) ls
checkSum n [] = 0

filter' :: ((Int, Int, Int) -> Bool) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
filter' f [] = []
filter' f [x]
    | f x = []
    | otherwise = [x]
filter' f (y:x:xs)
    | f y = y:filter' f (x:xs)
    | otherwise = (fst3 x, snd3 x, thd3 x + snd3 y):xs