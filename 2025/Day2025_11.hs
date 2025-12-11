module Day2025_11 where
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/11.txt"
    let graph = HM.fromList $ map parse $ lines contents
    -- part 1
    print $ pathsFromTo ((=="out") . fst) graph (HM.singleton ("you",S.empty) 1)
    -- part 2
    print $ pathsFromTo (==("out",importantNodes)) graph (HM.singleton ("svr",S.empty) 1)

parse :: String -> (String, [String])
parse s = let (c:cs) = words s in (init c,cs)

importantNodes :: S.Set String
importantNodes = S.fromList ["fft","dac"]

incWith :: String -> S.Set String -> S.Set String
incWith c state = if S.member c importantNodes then S.insert c state else state

pathsFromTo :: ((String,S.Set String) -> Bool) -> HM.HashMap String [String] -> HM.HashMap (String,S.Set String) Int -> Int
pathsFromTo goal graph ns = sum (HM.elems finished) + (if null prop then 0 else pathsFromTo goal graph prop)
    where
        finished = HM.filterWithKey (\k v -> goal k) ns
        unfinished = HM.filterWithKey (\k v -> fst k/="out") ns
        prop = HM.fromListWith (+) $ HM.foldrWithKey (\(c,s) n acc -> map ((,n) . (,incWith c s)) (HM.lookupDefault [] c graph) ++ acc) [] unfinished