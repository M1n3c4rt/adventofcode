module Day2015_19 where
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import System.Random (Random(randomR), mkStdGen, StdGen)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/19.txt"
    let
        rs = init $ init $ lines contents
        m = last $ lines contents
    -- part 1
    print $ length $ nub $ neighbours (HM.fromListWith (++) $ map parse rs) m
    -- part 2
    print $ head $ mapMaybe (\n -> reduceRandom (mkStdGen n) (HM.fromList $ map parse' rs) m) [0..]
    --print $ floodFillGoal (HM.fromList $ map parse' rs) "e" [m]

reduceRandom :: StdGen -> HM.HashMap String String -> String -> Maybe Int
reduceRandom gen table s =
    let reduced = reduce table s
        (n,gen') = randomR (0,length reduced-1) gen
    in if s == "e" then Just 0 else case reduced of
        [] -> Nothing
        _ -> (1+) <$> reduceRandom gen' table (reduced!!n) 

neighbours :: HM.HashMap String [String] -> String -> [String]
neighbours table [] = []
neighbours table s = let
    ks = HM.keys table
    ks' = filter (\k -> take (length k) s == k) ks
    in concatMap (\k -> map (++drop (length k) s) $ fromJust (HM.lookup k table)) ks' ++ map (head s:) (neighbours table (tail s))

reduce :: HM.HashMap String String -> String -> [String]
reduce table [] = []
reduce table s = let ks = filter (\k -> take (length k) s == k) (HM.keys table) in map (\k -> fromJust (HM.lookup k table) ++ drop (length k) s) ks ++ let (c:cs) = s in map (c:) $ reduce table cs 

parse :: String -> (String, [String])
parse l = let [a,_,b] = words l in (a,[b])

parse' :: String -> (String, String)
parse' l = let [a,_,b] = words l in (b,a)