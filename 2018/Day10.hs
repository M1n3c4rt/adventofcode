module Day10 where
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (isInfixOf)
import Control.Monad (when)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/10.txt"
    let ps = map parse $ lines contents
        mul = (\((x,y),(p,q)) -> (-(x `div` p)) - 200) $ head ps
        ps' = map (\((x,y),(p,q)) -> ((x+mul*p,y+mul*q),(p,q))) ps
    -- parts 1 & 2
    pprints 0 ps

pprints n ps = do
    let grid = S.fromList $ map fst ps
    if lineV 10 grid then do putStrLn $ pprint grid; print n else pprints (n+1) $ map (\((x,y),(p,q)) -> ((x+p,y+q),(p,q))) ps

parse :: String -> ((Int,Int),(Int,Int))
parse line = let [a,b,c,d,e] = concatMap (splitOn ">") $ splitOn "<" line in (read $ "("++b++")",read $ "("++d++")")

line n grid = any (\(x,y) -> all ((`S.member` grid) . (,y)) [x..x+n-1]) grid
lineV n grid = any (\(x,y) -> all ((`S.member` grid) . (x,)) [y..y+n-1]) grid

pprint :: S.Set (Int, Int) -> String
pprint points =
    let ((minx,miny),(maxx,maxy)) = ((minimum $ S.map fst points,minimum $ S.map snd points),(maximum $ S.map fst points,maximum $ S.map snd points))
    in unlines [[if (x,y) `S.member` points then '#' else ' ' | x <- [minx..maxx]] | y <- [miny..maxy]]