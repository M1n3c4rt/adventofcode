import Debug.Trace (trace)

main :: IO ()
main = do
    contents <- readFile "08.txt"
    let grid = parse contents
    -- part 1
    print $ length $ filter (isVisible grid) $ enumerate grid
    -- part 2
    print $ maximum $ map (scenicScore grid) $ enumerate grid

parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

enumerate :: [[a]] -> [(Int,Int,a)]
enumerate l = let el = map (zip [0..]) l in map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

isVisible :: [[Int]] -> (Int, Int, Int) -> Bool
isVisible grid (c,r,n) =
    let row = grid !! r
        col = map (!!c) grid
        (left,right) = (take c row,drop (c+1) row)
        (up,down) = (take r col,drop (r+1) col)
    in any (all (<n)) [left,right,up,down]

scenicScore :: [[Int]] -> (Int, Int, Int) -> Int
scenicScore grid (c,r,n) =
    let row = grid !! r
        col = map (!!c) grid
        (left,right) = (reverse $ take c row,drop (c+1) row)
        (up,down) = (reverse $ take r col,drop (r+1) col)
        score l = min (length l) $ 1 + length (takeWhile (<n) l)
    in product $ map score [left,right,up,down]