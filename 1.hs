import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.Char (toUpper)
import Data.List (sort, elemIndex, transpose, sortOn)
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)

main :: IO ()
main = do
    handle <- openFile "1.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    putStr $ show $ f $ transpose $ map getNumbers $ lines contents
    putStr "\n"
    -- part 2
    putStr $ show $ g $ transpose $ map getNumbers $ lines contents
    putStr "\n"
    hClose handle

getNumbers :: String -> [Int]
getNumbers s = map read $ words s :: [Int]

f :: [[Int]] -> Int
f [l1,l2] = sum $ map abs $ zipWith (-) (sort l1) (sort l2)

g :: [[Int]] -> Int
g [l1,l2] = sum $ map (\x -> x * numAppearances x l2) l1

numAppearances :: Int -> [Int] -> Int
numAppearances n l = length [a | a <- l, a == n]