import Data.List ( nub )
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "14.txt"
    -- part 1
    print $ splitQuads 101 103 [[],[],[],[]] $ map (transfer 100 101 103 . readPart) $ lines contents
    -- part 2
    print $ fst $ head $ filter (\x -> nub (snd x) == snd x) $ zip [1..] $ transferCumulative (101*103) 101 103 (map readPart $ lines contents)

readPart :: String -> ((Int,Int),(Int,Int))
readPart s = ((read $ takeWhile (/= ',') $ drop 2 s, read $ tail $ takeWhile (/= ' ') $ dropWhile (/= ',') s),(read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= 'v') s, read $ tail $ dropWhile (/= ',') $ tail $ dropWhile (/= ',') s))

transfer :: Int -> Int -> Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
transfer n len height ((a,b),(c,d)) = ((a+n*c) `mod` len, (b+n*d) `mod` height)

transferCumulative :: Int -> Int -> Int -> [((Int, Int), (Int, Int))] -> [[(Int, Int)]]
transferCumulative 0 len height ls = [map fst ls]
transferCumulative n len height ls = let new = map (transfer 1 len height) ls in new:transferCumulative (n-1) len height (zipWith (curry (Data.Bifunctor.second snd)) new ls)

splitQuads :: Int -> Int -> [[(Int,Int)]] -> [(Int,Int)] -> Int
splitQuads len height [k,l,m,n] ((a,b):ps)
    | a < len `div` 2 && b < height `div` 2 = splitQuads len height [(a,b):k,l,m,n] ps
    | a < len `div` 2 && b > height `div` 2 = splitQuads len height [k,(a,b):l,m,n] ps
    | a > len `div` 2 && b > height `div` 2 = splitQuads len height [k,l,(a,b):m,n] ps
    | a > len `div` 2 && b < height `div` 2 = splitQuads len height [k,l,m,(a,b):n] ps
    | otherwise = splitQuads len height [k,l,m,n] ps
splitQuads len height l [] = product $ map length l