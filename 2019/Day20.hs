module Day20 where
import Utility.AOC (enumerate, neighbours4, shortestDistance)
import qualified Data.HashMap.Strict as HM
import Data.Char (isUpper)
import Data.Maybe (fromJust, mapMaybe, maybeToList, isNothing)


main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/20.txt"
    let grid = HM.fromList $ map (\(x,y,c) -> ((x::Int,y::Int),c)) $ enumerate contents
    let (start,end,ctc) = coordsToCoords grid
    let [newstart,newend] = map (\(a,b) -> (a,b,0)) [start,end]
    -- part 1
    print $ fromJust $ shortestDistance (graph grid ctc) start end
    -- part 2
    print $ splitCtc ctc
    print $ cull $ map (\x -> shortestDistance (recursiveGraph 0 x grid ctc) newstart newend) [1..]

coordsToCoords :: HM.HashMap (Int,Int) Char -> ((Int,Int),(Int,Int),HM.HashMap (Int,Int) (Int,Int))
coordsToCoords grid =
    let helper (x,y) = ((x,y),filter (isUpper . snd) $ mapMaybe (\(p,q) -> ((p,q),) <$> HM.lookup (x+p,y+q) grid) $ neighbours4 (0,0))
        candidates = filter (not . null . snd) $ map helper $ HM.keys $ HM.filter (=='.') grid
        fillRest ((x,y),[((p,q),c)]) = case p+q of
            1 -> ((x,y),[c,c'])
            -1 -> ((x,y),[c',c])
            where c' = fromJust $ HM.lookup (x+2*p,y+2*q) grid
        coordsToPortals = map ((\(a,b) -> (b,[a])) . fillRest) candidates
        inverted = foldr (\(c,ns) acc -> HM.insertWith (++) c ns acc) HM.empty coordsToPortals
        start = head $ fromJust $ HM.lookup "AA" inverted
        end = head $ fromJust $ HM.lookup "ZZ" inverted
        flattened = HM.fromList $ concatMap swap $ HM.elems $ HM.delete "AA" $ HM.delete "ZZ" inverted
        swap [a,b] = [(a,b),(b,a)]
    in (start,end,flattened)

graph :: HM.HashMap (Int,Int) Char -> HM.HashMap (Int,Int) (Int,Int) -> HM.HashMap (Int,Int) [((Int,Int),Int)]
graph grid ctc =
    let neighbours = filter ((==Just '.') . (`HM.lookup` grid)) . neighbours4
        portNeighbour = maybeToList . (`HM.lookup` ctc)
        blanks = HM.filter (=='.') grid
    in HM.mapWithKey (\x _ -> map (,1) $ neighbours x ++ portNeighbour x) blanks

recursiveGraph :: Int -> Int -> HM.HashMap (Int,Int) Char -> HM.HashMap (Int,Int) (Int,Int) -> HM.HashMap (Int,Int,Int) [((Int,Int,Int),Int)]
recursiveGraph lvl limit grid ctc =
    let neighbours = map (\(a,b) -> (a,b,lvl)) . filter ((==Just '.') . (`HM.lookup` grid)) . neighbours4
        portNeighbour (x,y) = case port of
            Nothing -> []
            Just (a,b) -> case compare (abs (a-56) + abs (b-54)) (abs (x-56) + abs (y-54)) of
                LT -> [(a,b,lvl-1) | lvl /= 0]
                GT -> [(a,b,lvl+1) | lvl /= limit]
            where port = HM.lookup (x,y) ctc
        blanks = HM.filter (=='.') grid
        levelGraph = HM.mapKeys (\(a,b) -> (a,b,lvl)) $ HM.mapWithKey (\x _ -> map (,1) $ neighbours x ++ portNeighbour x) blanks
    in if lvl > limit then HM.empty else HM.union levelGraph (recursiveGraph (lvl+1) limit grid ctc)

cull :: Eq a => [Maybe a] -> a
cull (x:y:xs)
    | isNothing x || isNothing y = cull (y:xs)
    | x /= y = cull (y:xs)
    | otherwise = fromJust x

splitCtc = HM.filterWithKey (\(x,y) (a,b) -> (abs (a-56) + abs (b-54)) > (abs (x-56) + abs (y-54)))