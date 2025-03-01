import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

main :: IO ()
main = do
    contents <- readFile "09.txt"
    let grid = enumerate $ lines contents
    let valleys = HM.filterWithKey (filterValley grid) grid
    -- part 1
    print $ sum $ map (+1) $ HM.elems valleys
    -- part 2
    --print $ head $ map (floodFill S.empty grid . S.singleton) $ HM.keys valleys
    print $ product $ take 3 $ sortBy (comparing Data.Ord.Down) (map (floodFill S.empty grid . S.singleton) $ HM.keys valleys)


enumerate :: [[Char]] -> HM.HashMap (Int,Int) Int
enumerate l = let el = map (zip [0..]) l in HM.fromList $ map (\(a,b,c) -> ((b,a),read [c])) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

filterValley :: HM.HashMap (Int, Int) Int -> (Int, Int) -> Int -> Bool
filterValley cs (x,y) h =
    let neighbours = map (\p -> HM.lookupDefault 10 p cs) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    in all (>h) neighbours

floodFill :: S.Set (Int, Int) -> HM.HashMap (Int,Int) Int -> S.Set (Int, Int) -> Int
floodFill visited grid frontier
    | S.null frontier = S.size visited
    | otherwise = floodFill (S.union frontier visited) grid newFrontier
    where
        neighbours (x,y) = S.fromList [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        filterNeighbours p p' =
            let h = HM.lookupDefault 10 p grid
                h' = HM.lookupDefault 10 p' grid
            in h' < 9 && h' >= h && p' `S.notMember` visited && p' `S.notMember` frontier
        newFrontier = S.unions $ S.map (\p -> S.filter (filterNeighbours p) $ neighbours p) frontier