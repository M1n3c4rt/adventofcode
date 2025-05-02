import Utility.AOC (enumerateFilterSet)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "03.txt"
    let grid = enumerateFilterSet (=='#') contents
        height = length $ lines contents
        width = length $ head $ lines contents
    -- part 1
    print $ move height width grid (0,0) (3,1)
    -- part 2
    print $ product $ map (move height width grid (0,0)) [(1,1),(3,1),(5,1),(7,1),(1,2)]

move :: (Ord b, Integral a, Num b) => b -> a -> S.Set (a, b) -> (a, b) -> (a, b) -> Int
move h w grid (x,y) (p,q)
    | y >= h = 0
    | otherwise = fromEnum ((x,y) `S.member` grid) + move h w grid ((x+p) `mod` w,y+q) (p,q)