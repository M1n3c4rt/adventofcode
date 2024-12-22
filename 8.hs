import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List (nub)

main :: IO ()
main = do
    handle <- openFile "8.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    putStr $ show $ length $ nub $ concatMap (\x -> concatMap (antiNodes (length $ lines contents)) (pick2 $ getAnts x $ enumerate' $ lines contents)) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    putStr "\n"
    -- part 2
    putStr $ show $ length $ nub $ concatMap (\x -> concatMap (antiNodes' (length $ lines contents)) (pick2 $ getAnts x $ enumerate' $ lines contents)) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    putStr "\n"
    hClose handle

enumerate :: [Char] -> [(Int, Char)]
enumerate (l:ls) = (0,l):map (\x -> (fst x + 1, snd x)) (enumerate ls)
enumerate [] = []

enumerate' :: [[Char]] -> [(Int,Int,Char)]
enumerate' (l:ls) = map (\x -> (fst x,0,snd x)) (enumerate l) ++ map (\x -> (fst3 x, snd3 x + 1, thd3 x)) (enumerate' ls)
enumerate' [] = []

fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b
thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c

getAnts :: Char -> [(Int,Int,Char)] -> [(Int,Int)]
getAnts c l = map (\x -> (fst3 x, snd3 x)) $ filter (\x -> thd3 x == c) l

pick2 :: [a] -> [(a, a)]
pick2 (l:ls) = map (l,) ls ++ pick2 ls
pick2 [] = []

antiNodes :: Int -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
antiNodes n ((a,b),(c,d)) = let (p,q) = (c-a,d-b) in filter (\x -> fst x > -1 && fst x < n && snd x > -1 && snd x < n) [(a-p,b-q),(c+p,d+q)]

antiNodesP :: Int -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
antiNodesP n ((a,b),(c,d)) = let (p,q) = (c-a,d-b) in takeWhile (\x -> fst x > -1 && fst x < n && snd x > -1 && snd x < n) $ map (\k -> (a-k*p,b-k*q)) [0..]

antiNodesM :: Int -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
antiNodesM n ((a,b),(c,d)) = let (p,q) = (c-a,d-b) in takeWhile (\x -> fst x > -1 && fst x < n && snd x > -1 && snd x < n) $ map (\k -> (c+k*p,d+k*q)) [0..]

antiNodes' :: Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
antiNodes' n k = antiNodesP n k ++ antiNodesM n k