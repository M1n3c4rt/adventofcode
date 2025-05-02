import Data.List.Split (splitOn)
import Data.Char (isDigit, isAlphaNum)
import Data.List (elemIndices)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "22.txt"
    let (grid,dirs) = parse contents
    -- part 1
    print $ move False grid dirs
    -- part 2
    print $ move True grid dirs

data Dir = L | R deriving (Show)

parse :: String -> ([String], [(Dir, Int)])
parse contents = (lines a,helper ('R':takeWhile isAlphaNum b))
    where
        [a,b] = splitOn "\n\n" contents
        helper "" = []
        helper ss =
            let (dir,(num,rest)) = (head ss, span isDigit $ tail ss)
            in (getDir dir,read num):helper rest
        getDir c = case c of
            'R' -> R
            'L' -> L

next :: Int -> [String] -> (Int,Int) -> (Int,Int) -> ((Int,Int),(Int,Int))
next 0 grid (x,y) (d1,d2) = ((d1,d2),(x,y))
next n grid (x,y) (d1,d2)
    | inBounds (x+d1,y+d2) = case grid !! (y+d2) !! (x+d1) of
        '#' -> ((d1,d2),(x,y))
        '.' -> next (n-1) grid (x+d1,y+d2) (d1,d2)
    | otherwise = case grid !! y' !! x' of
        '#' -> ((d1,d2),(x,y))
        '.' -> next (n-1) grid (x',y') (d1,d2)
    where
        inBounds (x,y) = min x y >= 0 && y < length grid && x < length (grid !! y) && grid !! y !! x /= ' '
        (x',y') = last $ takeWhile inBounds $ iterate (\(a,b) -> (a-d1,b-d2)) (x,y)

next' :: Int -> [String] -> (Int,Int) -> (Int,Int) -> ((Int,Int),(Int,Int))
next' 0 grid (x,y) (d1,d2) = ((d1,d2),(x,y))
next' n grid (x,y) (d1,d2)
    | inBounds (x+d1,y+d2) = case grid !! (y+d2) !! (x+d1) of
        '#' -> ((d1,d2),(x,y))
        '.' -> next' (n-1) grid (x+d1,y+d2) (d1,d2)
    | otherwise = case grid !! y' !! x' of
        '#' -> ((d1,d2),(x,y))
        '.' -> next' (n-1) grid (x',y') (d1',d2')
    where
        inBounds (x,y) = min x y >= 0 && y < length grid && x < length (grid !! y) && grid !! y !! x /= ' '
        Just ((x',y'),(d1',d2')) = HM.lookup ((x,y),(d1,d2)) cubeMap

cubeMap :: HM.HashMap ((Int, Int), (Int, Int)) ((Int, Int), (Int, Int))
cubeMap = HM.fromList $ (\ls -> ls ++ map (\((p,(d1,d2)),(q,(d3,d4))) -> ((q,(-d3,-d4)),(p,(-d1,-d2)))) ls) $ concat
    [
        zipWith (\p q -> ((p,(1, 0)),(q,(0,-1)))) (map (49,) [150..199]) (map (,149) [50..99]), --red
        zipWith (\p q -> ((p,(1, 0)),(q,(-1,0)))) (map (99,) [100..149]) (map (149,) [49,48..0]), --orange
        zipWith (\p q -> ((p,(1, 0)),(q,(0,-1)))) (map (99,) [50..99]) (map (,49) [100..149]), --yellow
        zipWith (\p q -> ((p,(0,-1)),(q,(0,-1)))) (map (,0) [100..149]) (map (,199) [0..49]), --green
        zipWith (\p q -> ((p,(0,-1)),(q,(1, 0)))) (map (,0) [50..99]) (map (0,) [150..199]), --teal
        zipWith (\p q -> ((p,(-1,0)),(q,(1, 0)))) (map (50,) [0..49]) (map (0,) [149,148..100]), --blue
        zipWith (\p q -> ((p,(-1,0)),(q,(0, 1)))) (map (50,) [50..99]) (map (,100) [0..49]) --purple
    ]

move :: Bool -> [String] -> [(Dir,Int)] -> Int
move part2 grid dirs = 1000*(yl+1) + 4*(xl+1) + lastDir
    where
        start = (head $ elemIndices '.' $ head grid,0)
        startFacing = (0,-1)
        lastDir = case (d1,d2) of
            (1,0) -> 0
            (0,1) -> 1
            (-1,0) -> 2
            (0,-1) -> 3
        ((d1,d2),(xl,yl)) = foldl (\(d',c) (d,n) -> (if part2 then next' else next) n grid c (turn d d')) (startFacing,start) dirs
        turn L (d1,d2) = (d2,-d1)
        turn R (d1,d2) = (-d2,d1)