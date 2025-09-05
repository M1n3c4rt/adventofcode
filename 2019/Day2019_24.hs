module Day2019_24 where
import qualified Data.Set as S
import Utility.AOC (enumerateFilter, neighbours4)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/24.txt"
    -- part 1
    print $ biodiversityScore $ extrapolateFirst S.empty $ iterate step $ S.fromList $ enumerateFilter (=='#') contents
    -- part 2
    print $ S.size $ (!!200) $ iterate step' $ S.map (,0) $ S.fromList $ enumerateFilter (=='#') contents

step :: S.Set (Int, Int) -> S.Set (Int, Int)
step grid =
    let cells = S.fromList [(x,y) | x <- [0..4], y <- [0..4]]
        grid' = S.filter helper cells
        helper p = if S.member p grid then
            length (filter (`S.member` grid) $ neighbours4 p) == 1 else
            length (filter (`S.member` grid) $ neighbours4 p) `elem` [1,2]
    in grid'

extrapolateFirst :: Ord a => S.Set a -> [a] -> a
extrapolateFirst finished (x:xs)
    | x `S.member` finished = x
    | otherwise = extrapolateFirst (S.insert x finished) xs

biodiversityScore :: S.Set (Int, Int) -> Int
biodiversityScore = sum . S.map (\(x,y) -> 2^(5*y+x))

verticalBounds s = (minimum $ S.map snd s, maximum $ S.map snd s)

recNeighbours :: ((Int,Int),Int) -> [((Int,Int),Int)]
recNeighbours ((x,y),z)
    | (x,y) == (2,1) = map (,z) [(x+1,y),(x-1,y),(x,y-1)] ++ map ((,z+1) . (,0)) [0..4]
    | (x,y) == (1,2) = map (,z) [(x,y+1),(x-1,y),(x,y-1)] ++ map ((,z+1) . (0,)) [0..4]
    | (x,y) == (2,3) = map (,z) [(x+1,y),(x-1,y),(x,y+1)] ++ map ((,z+1) . (,4)) [0..4]
    | (x,y) == (3,2) = map (,z) [(x+1,y),(x,y+1),(x,y-1)] ++ map ((,z+1) . (4,)) [0..4]
    | otherwise = [
            if y == 0 then ((2,1),z-1) else ((x,y-1),z),
            if x == 0 then ((1,2),z-1) else ((x-1,y),z),
            if y == 4 then ((2,3),z-1) else ((x,y+1),z),
            if x == 4 then ((3,2),z-1) else ((x+1,y),z)
        ]

step' :: S.Set ((Int, Int), Int) -> S.Set ((Int, Int), Int)
step' grid =
    let (bmin,bmax) = verticalBounds grid
        cells = S.fromList [((x,y),z) | x <- [0..4], y <- [0..4], (x,y) /= (2,2), z <- [bmin-1..bmax+1]]
        grid' = S.filter helper cells
        helper p = if S.member p grid then
            length (filter (`S.member` grid) $ recNeighbours p) == 1 else
            length (filter (`S.member` grid) $ recNeighbours p) `elem` [1,2]
    in grid'