module Day2024_13 where

import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2024/13.txt"
    -- part 1
    print $ sum $ map (getTokens . readPart) $ splitOn "\n\n" contents
    -- part 2
    print $ sum $ map (getTokens' . readPart') $ splitOn "\n\n" contents
    
readPart :: String -> ((Int, Int), (Int, Int), (Int, Int)) 
readPart s = let a:b:c:cs = lines s in ((read $ take 2 $ drop 12 a, read $ take 2 $ drop 18 a),(read $ take 2 $ drop 12 b, read $ take 2 $ drop 18 b),(read $ takeWhile (/= ',') $ drop 9 c, read $ drop 2 $ dropWhile (/= 'Y') c))

readPart' :: String -> ((Int, Int), (Int, Int), (Int, Int)) 
readPart' s = let a:b:c:cs = lines s in ((read $ take 2 $ drop 12 a, read $ take 2 $ drop 18 a),(read $ take 2 $ drop 12 b, read $ take 2 $ drop 18 b),((+10000000000000) $ read $ takeWhile (/= ',') $ drop 9 c,(+10000000000000) $ read $ drop 2 $ dropWhile (/= 'Y') c))

getTokens :: ((Int, Int), (Int, Int), (Int, Int)) -> Int
getTokens ((a,b),(c,d),(e,f))
    | fst x > 100 || fst y > 100 = 0
    | snd x == 0 && snd y == 0 = 3*fst x + fst y
    | otherwise = 0
    where (x,y) = (divMod (c*f - d*e) (c*b - a*d), divMod (a*f - b*e) (a*d - b*c))

getTokens' :: ((Int, Int), (Int, Int), (Int, Int)) -> Int
getTokens' ((a,b),(c,d),(e,f))
    | snd x == 0 && snd y == 0 = 3*fst x + fst y
    | otherwise = 0
    where (x,y) = (divMod (c*f - d*e) (c*b - a*d), divMod (a*f - b*e) (a*d - b*c))