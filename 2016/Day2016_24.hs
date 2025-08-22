module Day2016_24 where
import qualified Data.HashMap.Strict as HM
import Utility.AOC (enumerateHM, neighbours4, choose, shortestDistanceWith, permute)
import Data.Maybe (mapMaybe, fromJust)
import Data.List (permutations)
import Debug.Trace (traceShowId, traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/24.txt"
    -- part 1
    let grid = enumerateHM contents
    print $ lookupTable grid
    print $ genStrings grid
    print $ minimum $ map (distanceFromOrder (lookupTable grid)) $ genStrings grid
    -- part 2
    print $ minimum $ map (distanceFromOrder (lookupTable grid)) $ genStrings' grid


pointsOfInterest :: HM.HashMap (Int,Int) Char -> [((Int,Int), Char)]
pointsOfInterest = HM.toList . HM.filter (`notElem`".#")

neighbours :: HM.HashMap (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
neighbours grid n = map fst $ filter ((/='#') . snd) $ mapMaybe (\k -> (k,) <$> HM.lookup k grid) $ neighbours4 n

lookupTable :: HM.HashMap (Int, Int) Char -> HM.HashMap (Char, Char) Int
lookupTable grid = HM.fromList $ map (\[(m,c),(n,d)] -> ((c,d),) $ fromJust $ shortestDistanceWith (map (,1) . neighbours grid) m n) $ permute 2 $ pointsOfInterest grid

distanceFromOrder :: HM.HashMap (Char, Char) Int -> String -> Int
distanceFromOrder lookup ord = sum $ zipWith (\a b -> fromJust $ HM.lookup (a,b) lookup) ord (tail ord)

genStrings :: HM.HashMap (Int, Int) Char -> [String]
genStrings grid = map ('0':) $ permutations $ filter (/='0') $ map snd $ pointsOfInterest grid

genStrings' :: HM.HashMap (Int, Int) Char -> [String]
genStrings' grid = map (\l -> '0':l ++ "0") $ permutations $ filter (/='0') $ map snd $ pointsOfInterest grid