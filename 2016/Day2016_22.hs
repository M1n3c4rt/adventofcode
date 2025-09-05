{-# LANGUAGE FlexibleContexts #-}
module Day2016_22 where
import Utility.AOC (numbers', permute, neighbours4, prettyPrintHM)
import qualified Data.HashMap.Strict as HM
import Data.List.Extra (nub)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/22.txt"
    -- part 1
    print $ length $ filter (\[(_,a),(_,b)] -> viable a b) $ permute 2 $ map parse $ drop 2 $ lines contents
    -- part 2
    -- accidentally solved this by hand
    let nodes :: HM.HashMap (Int,Int) (Int,Int) = HM.fromList $ map parse $ drop 2 $ lines contents
    putStrLn $ prettyPrintHM $ HM.mapWithKey (\k (u,a) -> if u == 0 then '_' else if u > 200 then '#' else if k == (31,0) then 'G' else if k == (0,0) then 'S' else '.') nodes
    --print $ floodFill S.empty $ S.singleton ((31,0),nodes)

type State = ((Int,Int),HM.HashMap (Int,Int) (Int,Int))

parse l = let [x,y,_,u,a,_] = numbers' l in ((x,y),(u,a))

viable (u,a) (u',a') = u /= 0 && u <= a'
viable' (u,a) (u',a') = u /= 0 && u <= a' && u' == 0

neighbours :: State -> [State]
neighbours (goal,nodes) = nub $ map adjustState candidates
    where
        candidates = concatMap (\(k,v) -> map (k,) v) $ HM.toList $ HM.mapWithKey helper nodes
        adjustState (a,b) =
            let Just (ua,aa) = HM.lookup a nodes
                Just (ub,ab) = HM.lookup b nodes
            in (if a == goal then (b,) else (goal,)) $ HM.insert a (0,ua+aa) $ HM.insert b (ub+ua,ab-ua) nodes
        helper k v = filter (\k' -> case HM.lookup k' nodes of Just v' -> (if v' == goal then viable' else viable) vk v'; Nothing -> False) $ neighbours4 k
            where Just vk = HM.lookup k nodes

floodFill :: S.Set State -> S.Set State -> Int
floodFill finished frontier
    | any ((==(0,0)) . fst) frontier = 0
    | otherwise = 1 + floodFill (S.union finished frontier) (S.filter (\x -> S.notMember x frontier && S.notMember x finished) $ S.unions $ S.map (S.fromList . neighbours) frontier)