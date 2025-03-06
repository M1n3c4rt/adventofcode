import Data.List (groupBy)
import Data.Char (isDigit)

main :: IO ()
main = do
    contents <- readFile "17.txt"
    let [xmin,xmax,ymin,ymax] = parse contents
    -- part 1
    print $ part1 ymax ymin
    -- part 2
    --print $ boundY [0..2000] ymax ymin
    --print $ boundY [0,-1..(-2000)] ymax ymin
    --print $ boundX [100,99..0] xmax xmin
    --print $ boundX [0..1000] xmax xmin
    print $ length $ filter (validate xmin xmax ymin ymax) [(vx,vy) | vy <- [-200..100], vx <- [0..500]]

genDistancesY :: Int -> Int -> Int -> [Int]
genDistancesY m vy y = if y < m then [] else y:genDistancesY m (vy-1) (y+vy)

genDistancesX :: Int -> Int -> [Int]
genDistancesX vx x = if vx == 0 then repeat x else x:genDistancesX (vx-1) (x+vx)

parse :: String -> [Int]
parse ss = map read $ [(!!1),(!!3),(!!5),(!!7)] <*> pure (groupBy (\a b -> isNum a == isNum b) ss)
    where isNum c = isDigit c || c == '-'

part1 :: Int -> Int -> Int
part1 ymax ymin = maximum $ last $ filter (any (\x -> x >= ymin && x <= ymax)) $ map (\v -> genDistancesY (-100) v 0) [0..500]

boundY :: [Int] -> Int -> Int -> Int
boundY range ymax ymin = (!!2) $ last $ filter (any (\x -> x >= ymin && x <= ymax)) $ map (\v -> genDistancesY (-1000) v 0) range

boundX :: [Int] -> Int -> Int -> Int
boundX range xmax xmin = (!!2) $ last $ filter (any (\x -> x >= xmin && x <= xmax)) $ map (`genDistancesX` 0) range

validate :: Int -> Int -> Int -> Int -> (Int, Int) -> Bool
validate xmin xmax ymin ymax (vx,vy) = any (\(x,y) -> x >= xmin && x <= xmax && y >= ymin && y <= ymax) $ zip (genDistancesX vx 0) (genDistancesY (-100) vy 0)