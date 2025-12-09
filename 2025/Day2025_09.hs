module Day2025_09 where
import Utility.AOC (choose)
import Data.List.Extra (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/09.txt"
    -- part 1
    let points = map parse $ lines contents
    let redRects = map (\[a,b] -> (min a b,max a b)) (choose 2 points)
    print $ maximum $ map area redRects
    -- part 2
    let ls = pointsToLines points
    print $ maximum $ map (area . (\[a,b,c,d] -> (a,c))) $ filter (all (inLines ls)) $ filter (\r -> inRects (pointsToLines r) ls) $ map rectToPoints redRects

type Point = (Int,Int)
type Line = ((Int,Int),(Int,Int))
type Rect = ((Int,Int),(Int,Int))

intersectLines :: Line -> Line -> Bool
intersectLines l1@((a,b),(c,d)) l2@((e,f),(g,h))
    | b == d && e == g = min a c < e && e < max a c && min f h < b && b < max f h
    | a == c && f == h = intersectLines l2 l1
    | otherwise = False

intersectLineRay :: Bool -> Point -> Line -> Bool
intersectLineRay right p@(x,y) l1@((a,b),(c,d))
    | a == c = min b d <= y && y <= max b d && (if right then max a c >= x else min a c <= x)
    | otherwise = b == y && (if right then max a c >= x else min a c <= x)

rectToPoints :: Rect -> [Point]
rectToPoints ((a,b),(c,d)) = [(a,b),(a,d),(c,d),(c,b)]

pointsToLines :: [Point] -> [Line]
pointsToLines ls = zip ls $ tail $ cycle ls

onLine :: Line -> Point -> Bool
onLine ((x1,y1),(x2,y2)) (xp,yp)
    | x1 == x2 && x1 == xp = (y1-yp)*(y2-yp) <= 0
    | y1 == y2 && y1 == yp = (x1-xp)*(x2-xp) <= 0
    | otherwise = False

inLines :: [(Point,Point)] -> Point -> Bool
inLines ls p = any (`onLine` p) ls || length (filter (intersectLineRay True p) ls) * length (filter (intersectLineRay False p) ls) /= 0

inRects :: [Line] -> [Line] -> Bool
inRects rs ls = null [(x,y) | x <- rs, y <- ls, intersectLines x y]

parse :: String -> Point
parse = (\[a,b] -> (a,b)) . map read . splitOn ","

area :: Rect -> Int
area ((a,b),(c,d)) = (abs (a-c) + 1) * (abs (b-d) + 1)