import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Control.Monad (foldM)

main :: IO ()
main = do
    handle <- openFile "7.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    putStr $ show $ sum $ map fst $ filter (\x -> fst x `elem` uncurry f x) $ map getNumbers $ lines contents
    putStr "\n"
    -- part 2
    putStr $ show $ sum $ map fst $ filter (\x -> fst x `elem` uncurry g x) $ map getNumbers $ lines contents
    putStr "\n"
    hClose handle

getNumbers :: String -> (Int, [Int])
getNumbers s = let [a,b] = splitOn ":" s in (read a, map read $ words b)

f :: Int -> [Int] -> [Int]
f n (x:y:ys) = concat $ filter (\x -> null x || head x <= n) [f n ((x+y):ys),f n ((x*y):ys)]
f n [x] = [x]
f n [] = []

(~) :: Int -> Int -> Int
(~) x y = read $ show x ++ show y

g :: Int -> [Int] -> [Int]
g n (x:y:ys) = concat $ filter (\x -> null x || head x <= n) [g n ((x+y):ys),g n ((x*y):ys),g n ((x~y):ys)]
g n [x] = [x]
g n [] = []