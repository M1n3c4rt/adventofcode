module Day2018_15 where
import Utility.AOC (enumerateHM, enumerateFilterSet, taxicab2, neighbours4, enumerateFilter)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.List (sort, sortOn, transpose)
import Debug.Trace (traceShow, traceShowId)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/15.txt"
    let contents' = unlines $ transpose $ lines contents
        walls = enumerateFilterSet (=='#') contents'
        goblins = enumerateFilter (=='G') contents'
        elves = enumerateFilter (=='E') contents'
        units = HM.union (HM.fromList (map (,(False,200)) goblins)) (HM.fromList (map (,(True,200)) elves))
    -- part 1
    print $ snd $ fight 3 (-1) walls units []
    -- part 2
    print $ snd $ head $ dropWhile ((<length elves) . fst) $ map (\x -> fight x (-1) walls units []) [4..]

type Units = HM.HashMap (Int,Int) (Bool,Int)

turn :: Int -> S.Set (Int,Int) -> Units -> Bool -> (Int,Int) -> Units
turn elfPower walls units side p = newUnits
    where
        allies = HM.filter ((==side) . fst) units
        enemies = HM.filter ((/=side) . fst) units

        [as,es] = map (S.fromList . HM.keys) [allies,enemies]

        inRange = S.unions $ S.map (S.fromList . neighbours4) es
        moved =
            let target = snd <$> floodFillNearest 0 inRange (S.unions [walls,as,es]) (S.singleton p) S.empty
                neighbourdists = mapMaybe (\x -> (x,) . fst <$> floodFillNearest 0 (S.singleton (fromJust target)) (S.unions [walls,as,es]) (S.singleton x) S.empty) $ sort $ filter (`S.notMember` S.unions [walls,es,as]) $ neighbours4 p
                candidate = case target of
                    Just t -> fst <$> safeMin (compare `on` snd) neighbourdists
                    Nothing -> Nothing
            in if Just p == target then p else fromMaybe p candidate
        combatInRange = safeMin (compare `on` (\x -> (x `HM.lookup` enemies,x))) $ S.filter (`elem` neighbours4 moved) es
        safeMin f xs = if null xs then Nothing else Just (minimumBy f xs)
        newUnits = case combatInRange of
            Just c -> HM.filter ((>0) . snd) $ HM.adjust (\(b,n) -> (b,n-if b then 3 else elfPower)) c $ HM.mapKeys (\k -> if k == p then moved else k) units
            Nothing -> HM.filter ((>0) . snd) $ HM.mapKeys (\k -> if k == p then moved else k) units

fight :: Int -> Int -> S.Set (Int,Int) -> Units -> [((Int,Int),(Bool,Int))] -> (Int,Int)
fight elfPower n walls units (a@(p,(b,m)):queue)
    | all fst units = (HM.size $ HM.filter fst units,(n + if null queue then 1 else 0) * sum (HM.map snd units))
    | not (any fst units) = (0,(n + if null queue then 1 else 0) * sum (HM.map snd units))
    | otherwise = let new = turn elfPower walls units b p in fight elfPower n walls new $ filter ((`elem` HM.keys new) . fst) queue
fight elfPower n walls units [] = fight elfPower (n+1) walls units $ sortOn fst $ HM.toList units

floodFillNearest :: Int -> S.Set (Int,Int) -> S.Set (Int,Int) -> S.Set (Int,Int) -> S.Set (Int,Int) -> Maybe (Int,(Int,Int))
floodFillNearest n targets walls frontier finished
    | any (`S.member` targets) frontier = Just $ (n,) $ minimum $ S.toList $ S.filter (`S.member` targets) frontier
    | S.null frontier = Nothing
    | otherwise = floodFillNearest (n+1) targets walls (S.filter (\x -> x `S.notMember` finished && x `S.notMember` frontier && x `S.notMember` walls) $ S.unions $ S.map (S.fromList . neighbours4) frontier) (S.union frontier finished)