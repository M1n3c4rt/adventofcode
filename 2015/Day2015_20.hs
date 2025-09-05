module Day2015_20 where
import Data.List (nub)
import Debug.Trace (traceShow, traceShowId)
import qualified Data.HashMap.Strict as HM
import Data.List.Extra (maximumOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/20.txt"
    -- part 1
    print $ last $ filter ((>=read contents `div` 10) . sum . presents . traceShowId) [1000000,999999..1]
    -- part 2
    print $ last $ filter ((>=33100000) . presents' . traceShowId) [1000000,999999..1]
    --print $ fst $ maximumOn snd $ HM.toList $ snd $ head $ dropWhile ((<read contents) . maximum . snd) $ iterate (\(k,m) -> (k+1,HM.unionWith (+) (houseMap k) m)) (2,houseMap 1)

presents :: Int -> [Int]
presents house = concatMap (\k -> nub [k,house`div`k]) $ filter (\n -> house `mod` n == 0) [1 .. floor $ sqrt $ fromIntegral house]

presents' :: Int -> Int
presents' house = (11*) $ sum $ filter (\n -> house `div` n <= 50) $ presents house

houseMap :: Int -> HM.HashMap Int Int
houseMap n = HM.fromList $ map (\k -> (n*k,11*n)) [1..50]