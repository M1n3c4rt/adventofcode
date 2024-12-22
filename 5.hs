import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List (transpose, sort, sortBy, sortOn)
import Data.List.Split ( splitOn, splitOneOf )
import qualified Data.Bifunctor

main :: IO ()
main = do
    handle <- openFile "5.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    let (l,u) = format contents in putStr $ show $ sum $ map getMiddle $ filter (\x -> sortUpdate l x == x) u
    putStr "\n"
    -- part 2
    let (l,u) = format contents in putStr $ show $ sum $ map (getMiddle . sortUpdate l) $ filter (\x -> sortUpdate l x /= x) u
    putStr "\n"
    hClose handle

getNumbers :: String -> [Int]
getNumbers s = map read $ splitOneOf "|," s :: [Int]
tuplify2 :: [a] -> (a, a)
tuplify2 [x,y] = (x,y)

format :: String -> ([(Int,Int)],[[Int]])
format c = Data.Bifunctor.first (map tuplify2) $ tuplify2 $ map (map getNumbers . lines) (splitOn "\n\n" c)

getEntries :: [(Int,Int)] -> [Int] -> Int -> Int
getEntries l ns n = length $ filter id $ map (\x -> fst x == n && snd x `elem` ns) l

sortUpdate :: [(Int, Int)] -> [Int] -> [Int]
sortUpdate l ns = sortOn ((0-) . getEntries l ns) ns

getMiddle :: [Int] -> Int
getMiddle l = l !! ((length l - 1) `div` 2)