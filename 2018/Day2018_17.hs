module Day2018_17 where
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (nub)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/17.txt"
    let clay = S.unions $ map (S.fromList . parse) $ lines contents
    let water = fall True (maximum $ S.map snd clay) clay S.empty S.empty [(500,minimum $ S.map snd clay)]
    -- part 1
    print $ S.size $ uncurry S.union water
    print $ S.size $ fst water

parse :: String -> [(Int,Int)]
parse (l:_:line) = let [ls,_:_:rest] = splitOn ", " line in map ((if l == 'x' then (,) else flip (,)) $ read ls) $ let [a,b] = splitOn ".." rest in [read a..read b]

fall :: Bool -> Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int) -> [(Int, Int)] -> (S.Set (Int, Int), S.Set (Int, Int))
fall True lower clay water unfinished [] = (water,unfinished)
fall False lower clay water unfinished [] = fall True lower clay water S.empty $ S.toList unfinished
fall isFinished lower clay water unfinished ((x,y):fs)
    | y+1 > lower = fall isFinished lower clay water (S.insert (x,y) unfinished) fs
    | (x,y+1) `S.member` clay || (x,y+1) `S.member` water =
        case (safe (x,y) (-1), safe (x,y) 1) of
            (Right (a,b),Right (c,d)) -> fall False lower clay (S.union (S.fromList $ map (,b) [a..c]) water) unfinished fs
            (p,q) -> let line = map (,y) [fst (extract p) .. fst (extract q)] in if all (\x' -> x' `elem` fs || x' `S.member` unfinished || fst x' == x) line then fall isFinished lower clay water (S.insert (x,y) unfinished) fs else fall False lower clay water (S.union (S.fromList ((x,y):line)) unfinished) fs 
    | (x,y+1) `elem` fs || (x,y+1) `S.member` unfinished = fall isFinished lower clay water (S.insert (x,y) unfinished) fs
    | otherwise = let (x',y') = safeV (x,y) in fall False lower clay water (S.union (S.fromList $ map (x,) [y..y']) unfinished) (map (x,) [y+1..y'] ++ fs)
    where
            safeV (x,y)
                | y == lower = (x,y)
                | (x,y+1) `S.member` clay || (x,y+1) `S.member` water = (x,y)
                | otherwise = safeV (x,y+1)
            safe (x, y) d
                | (x, y + 1) `S.notMember` clay && (x, y + 1) `S.notMember` water = Left (x, y)
                | (x + d, y) `S.member` clay || (x + d, y) `S.member` water = Right (x, y)
                | otherwise = safe (x + d, y) d
            extract (Right a) = a
            extract (Left a) = a