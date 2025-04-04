import Data.Numbers.Primes (isPrime)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Data.Foldable (minimumBy)
import Data.List (sortOn)
import Debug.Trace (traceShow)
import Data.Ord (Down(Down))

main :: IO ()
main = do
    contents <- readFile "13.txt"
    let buses = mapMaybe (\x -> if x == "x" then Nothing else Just (read x)) $ splitOn "," $ (!!1) $ lines contents
        buses' = sortOn (Down . fst) (mapMaybe (\(n,x) -> if x == "x" then Nothing else Just (read x,(read x - (n `mod` read x)) `mod` read x)) $ zip [0..] $ splitOn "," $ (!!1) $ lines contents) :: [(Int,Int)]
        time = read $ head $ lines contents
    -- part 1
    print $ (\(a,b) -> a*(b-time)) $ minimumBy (compare `on` snd) $ map (\x -> (x,nextMultiple time x)) buses
    -- part 2
    print $ crt 1 1 buses'

nextMultiple :: Integral a => a -> a -> a
nextMultiple x k = x + (k - (x `mod` k))

crt :: (Integral d, Show d) => d -> d -> [(d, d)] -> d
crt s step ((n,k):ns)
    | s `mod` n == k = if null ns then s else traceShow (s,step,n,k) $ crt s (step*n) ns
    | otherwise = traceShow (s,step,n,k) $ crt (s+step) step ((n,k):ns)