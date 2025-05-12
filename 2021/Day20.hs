module Day20 where

import Data.List.Split (splitOn)
import Data.List (elemIndices)
import qualified Data.Set as S
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2021/20.txt"
    let (alg,image) = parse contents
    -- part 1
    print $ S.size $ iterateParity 2 (enhance alg) True image
    -- part 2
    print $ S.size $ iterateParity 50 (enhance alg) True image

parse :: [Char] -> (S.Set Int, S.Set (Int, Int))
parse ss =
    let [a,i] = splitOn "\n\n" ss
        alg = S.fromList $ elemIndices '#' a
        image = S.fromList $ enumerate $ lines i
    in (alg,image)

enumerate :: [[Char]] -> [(Int, Int)]
enumerate l =
    let el = map (zip [0..]) l
    in map (\(a,b,c) -> (a,b)) $ filter (\(a,b,c) -> c == '#') $ map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

bounds :: S.Set (Int, Int) -> ((Int, Int), (Int, Int))
bounds image =
    let xs = S.map fst image
        ys = S.map snd image
    in ((minimum xs,minimum ys),(maximum xs, maximum ys))

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

enhance :: S.Set Int -> Bool -> S.Set (Int, Int) -> S.Set (Int, Int)
enhance alg parity image =
    let ((xm,ym),(xn,yn)) = bounds image
        binToDec ns = sum $ zipWith (*) (map fromEnum ns) (map (2^) [8,7..0])
        outOfBounds (x,y) = x < xm || x > xn || y < ym || y > yn
        helper p = (`S.member` alg) $ binToDec $ map (\x -> x `S.member` image || (parity && outOfBounds x)) $ neighbours p
    in S.filter (\(x,y) -> not (x < -150 || x > 249 || y < -150 || y > 249)) $ S.fromList $ filter helper [(x,y) | x <- [xm-1..xn+1], y <- [ym-1..yn+1]]

pprint :: S.Set (Int, Int) -> String
pprint image = let ((xm,ym),(xn,yn)) = bounds image in unlines [[if (x,y) `S.member` image then '#' else '.' | x <- [xm..xn]] | y <- [ym..yn]]

pprint' :: ((Int, Int), (Int, Int)) -> S.Set (Int, Int) -> String
pprint' ((xm,ym),(xn,yn)) image = unlines [[if (x,y) `S.member` image then '#' else '.' | x <- [xm..xn]] | y <- [ym..yn]]

iterateParity :: (Eq t1, Num t1) => t1 -> (Bool -> t2 -> t2) -> Bool -> t2 -> t2
iterateParity 0 _ _ x = x
iterateParity n f b x = f b (iterateParity (n-1) f (not b) x)