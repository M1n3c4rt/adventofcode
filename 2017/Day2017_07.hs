module Day2017_07 where
import qualified Data.HashMap.Strict as HM
import Debug.Trace (traceShowId, traceShow)
import Data.List (sortOn, groupBy)
import Data.Function (on)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/07.txt"
    let s = head $ findBottom $ map parse $ lines contents
    -- part 1
    putStrLn s
    -- part 2
    print $ (`discrepancy` s) $ HM.fromList $ map parse $ lines contents

parse :: String -> (String, (Int, [String]))
parse line = case words line of
    x:y:z:zs -> (x,(read $ init $ tail y,map (filter (/=',')) zs))
    [x,y] -> (x,(read $ init $ tail y,[]))

findBottom :: Eq a1 => [(a1, (a2, [a1]))] -> [a1]
findBottom ps = filter (`notElem` concatMap (snd . snd) ps) (map fst ps)

weight ps s = let Just (n,ps') = HM.lookup s ps in n + sum (map (weight ps) ps')

discrepancy :: HM.HashMap String (Int,[String]) -> String -> Maybe Int
discrepancy ps s = if null ps' then Nothing else case unique (traceShow (s,ps') $ traceShowId $ map (\p -> (p,weight ps p)) ps') of
        Nothing -> Nothing
        Just ((s',n'),k) -> case discrepancy ps s' of
            Nothing -> Just ((k-n'+) $ fst $ fromJust $ HM.lookup s' ps)
            Just k' -> Just k'
    where
        Just (n,ps') = HM.lookup s ps
        candidates = map (\p -> (p,weight ps p)) ps'

unique ns = case sortOn length $ groupBy ((==) `on` snd) $ sortOn snd ns of
    [] -> Nothing
    [x] -> Nothing
    x:y:ys -> Just (head x,snd $ head y)