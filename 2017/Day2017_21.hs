module Day2017_21 where
import Data.List (transpose)
import Data.List.Extra (splitOn)
import Utility.AOC (chunk)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/21.txt"
    let table = HM.fromList $ complete $ map parse $ lines contents
    -- part 1
    print $ length $ concatMap (filter (=='#')) $ (!!5) $ iterate (expand table) original
    -- part 2
    print $ length $ concatMap (filter (=='#')) $ (!!18) $ iterate (expand table) original

type Grid = [[Char]]

original :: Grid
original = [".#.","..#","###"]

parse :: String -> (Grid,Grid)
parse = (\[a,b] -> (a,b)) . map (splitOn "/") . splitOn " => "

complete :: [(Grid,Grid)] -> [(Grid,Grid)]
complete = concatMap (\(a,b) -> map (,b) $ rots a)

rots :: Grid -> [Grid]
rots l =
    [
        id,
        transpose,
        reverse,
        map reverse,
        transpose . map reverse,
        transpose . reverse,
        reverse . map reverse,
        transpose . reverse . map reverse
    ] <*> [l]

expand :: HM.HashMap Grid Grid -> Grid -> Grid
expand m l = concatMap (map concat . transpose . map (fromJust . (`HM.lookup` m)) . transpose . map (chunk size)) (chunk size l)
    where size = if even (length l) then 2 else 3

chunks :: Grid -> [[Grid]]
chunks l = map (transpose . map (chunk size)) (chunk size l)
    where size = if even (length l) then 2 else 3