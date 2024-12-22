{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Data.List (minimumBy, nub)
import Data.Function (on)

main :: IO ()
main = do
    handle <- openFile "14.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    putStr $ show $ splitQuads 101 103 [[],[],[],[]] $ map (transfer 100 101 103 . readPart) $ lines contents
    putStr "\n"
    -- part 2
    putStr $ show $ fst $ head $ filter (\x -> nub (snd x) == snd x) $ zip [1..] $ transferCumulative (101*103) 101 103 (map readPart $ lines contents)
    putStr "\n"
    hClose handle

--p=20,2 v=42,-26
readPart :: String -> ((Int,Int),(Int,Int))
readPart s = ((read $ takeWhile (/= ',') $ drop 2 s, read $ tail $ takeWhile (/= ' ') $ dropWhile (/= ',') s),(read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= 'v') s, read $ tail $ dropWhile (/= ',') $ tail $ dropWhile (/= ',') s))

transfer :: Int -> Int -> Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
transfer n len height ((a,b),(c,d)) = ((a+n*c) `mod` len, (b+n*d) `mod` height)

transferCumulative :: Int -> Int -> Int -> [((Int, Int), (Int, Int))] -> [[(Int, Int)]]
transferCumulative 0 len height ls = [map fst ls]
transferCumulative n len height ls = let new = map (transfer 1 len height) ls in new:transferCumulative (n-1) len height (zipWith (curry (\x -> (fst x, snd $ snd x))) new ls)

splitQuads :: Int -> Int -> [[(Int,Int)]] -> [(Int,Int)] -> Int
splitQuads len height [k,l,m,n] ((a,b):ps)
    | a < len `div` 2 && b < height `div` 2 = splitQuads len height [(a,b):k,l,m,n] ps
    | a < len `div` 2 && b > height `div` 2 = splitQuads len height [k,(a,b):l,m,n] ps
    | a > len `div` 2 && b > height `div` 2 = splitQuads len height [k,l,(a,b):m,n] ps
    | a > len `div` 2 && b < height `div` 2 = splitQuads len height [k,l,m,(a,b):n] ps
    | otherwise = splitQuads len height [k,l,m,n] ps
splitQuads len height l [] = product $ map length l