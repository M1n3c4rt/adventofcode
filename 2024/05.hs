
import Data.List ( sortOn )
import Data.List.Split ( splitOn, splitOneOf )
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "05.txt"
    let (l,u) = format contents
    -- part 1
    print $ sum $ map getMiddle $ filter (\x -> sortUpdate l x == x) u
    -- part 2
    print $ sum $ map (getMiddle . sortUpdate l) $ filter (\x -> sortUpdate l x /= x) u
    
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