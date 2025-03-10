import Data.List (transpose)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "24.txt"
    let (height,width,blocks) = parse contents
    let gs = getGraphs height width blocks
    -- part 1
    print $ floodFill' 1 (S.singleton (0,-1)) [(width-1,height)] $ tail gs
    -- part 2
    print $ floodFill' 1 (S.singleton (0,-1)) [(width-1,height),(0,-1),(width-1,height)] $ tail gs

parse :: String -> (Int, Int, [S.Set (Int, Int)])
parse contents =
    let grid = map (tail . init) . (tail . init) $ lines contents
        height = length grid
        width = length $ head grid
        enumerate l = let el = map (zip [0..]) l in map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el
        grid' = enumerate grid
        helper (x,y,c) = case c of
            '.' -> []
            '>' -> map ((,y) . (`mod` width)) [x..]
            '<' -> map ((,y) . (`mod` width)) [x,x-1..]
            'v' -> map ((x,) . (`mod` height)) [y..]
            '^' -> map ((x,) . (`mod` height)) [y,y-1..]
        blocks = map S.fromList $ transpose $ map helper grid'
    in (height,width,blocks)

getGraphs :: Int -> Int -> [S.Set (Int,Int)] -> [HM.HashMap (Int,Int) (S.Set (Int,Int))]
getGraphs height width (b:next:blocks) =
    let grid = (0,-1):(width-1,height):[(x,y) | x <- [0..width], y <- [0..height], (x,y) `S.notMember` b]
        inBounds (x,y) = (x,y) `elem` [(width-1,height),(0,-1)] || (min x y >= 0 && x < width && y < height)
        helper (x,y) = S.filter (`S.notMember` next) $ S.filter inBounds $ S.fromAscList [(x-1,y),(x,y-1),(x,y),(x,y+1),(x+1,y)]
    in HM.fromList (map (\c -> (c,helper c)) grid):getGraphs height width (next:blocks)

floodFill :: Int -> S.Set (Int,Int) -> (Int,Int) -> [HM.HashMap (Int,Int) (S.Set (Int,Int))] -> Int
floodFill t srcs dest (g:gs)
--    | dest `S.member` srcs = t
    | dest `S.member` srcs = t
    | otherwise =
        let nexts = S.unions $ S.map (\src -> HM.lookupDefault S.empty src g) srcs
--        in floodFill (t+1) nexts dest gs
        in floodFill (t+1) nexts dest gs

floodFill' :: Int -> S.Set (Int,Int) -> [(Int,Int)] -> [HM.HashMap (Int,Int) (S.Set (Int,Int))] -> Int
floodFill' t srcs [] _ = t
floodFill' t srcs dests@(d:ds) (g:gs)
    | d `S.member` srcs = floodFill' t (S.singleton d) ds (g:gs)
    | otherwise =
        let nexts = S.unions $ S.map (\src -> HM.lookupDefault S.empty src g) srcs
        in floodFill' (t+1) nexts dests gs

pprint :: S.Set (Int, Int) -> Int -> Int -> String
pprint srcs height width = unlines $ map (zipWith (curry (\c -> if c `S.member` srcs then '#' else '.')) [0..width-1] . repeat) [0..height-1]