import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List (nub)
import Data.Char (digitToInt)

main :: IO ()
main = do
    handle <- openFile "10.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    let l = readContents contents in putStr $ show $ sum $ map (step (length l) 0 l) $ nest $ findZeroes' 0 l
    putStr "\n"
    -- part 2
    let l = readContents contents in putStr $ show $ sum $ map (step' (length l) 0 l) $ nest $ findZeroes' 0 l
    putStr "\n"
    hClose handle

step :: Int -> Int -> [[Int]] -> [(Int,Int)] -> Int
step k 9 l' l = length l
step k n l' l = step k (n+1) l' $ nub $ filter (\x -> l' !! fst x !! snd x == n+1) $ clip k $ concatMap neighbours l

step' :: Int -> Int -> [[Int]] -> [(Int,Int)] -> Int
step' k 9 l' l = length l
step' k n l' l = step' k (n+1) l' $ filter (\x -> l' !! fst x !! snd x == n+1) $ clip k $ concatMap neighbours l

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (a,b) = [(a+1,b),(a-1,b),(a,b+1),(a,b-1)]

clip :: Int -> [(Int, Int)] -> [(Int, Int)]
clip n = filter (\x -> uncurry min x >= 0 && uncurry max x < n)

findZeroes :: Int -> [Int] -> [Int]
findZeroes n [] = []
findZeroes n (l:ls)
    | l == 0 = n:findZeroes (n+1) ls
    | otherwise = findZeroes (n+1) ls

findZeroes' :: Int -> [[Int]] -> [(Int, Int)]
findZeroes' n (ls:lss) = map (n,) (findZeroes 0 ls) ++ findZeroes' (n+1) lss
findZeroes' n [] = []

readContents :: String -> [[Int]]
readContents s = map (map digitToInt) $ lines s

nest :: [a] -> [[a]]
nest = foldr (\ l -> (:) [l]) []