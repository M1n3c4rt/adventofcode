module Day2019_12 where

import Data.List (groupBy, findIndices)
import Data.Function (on)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/12.txt"
    let ss = iterate step $ parse contents
        xs = map (map (\((a,b,c),(x,y,z)) -> x)) ss
        ys = map (map (\((a,b,c),(x,y,z)) -> y)) ss
        zs = map (map (\((a,b,c),(x,y,z)) -> z)) ss
    -- part 1
    print $ energy $ ss !! 1000
    -- part 2
    print $ foldl1 lcm $ map ((!!2) . findIndices (all (==0))) [xs,ys,zs]

parse :: String -> [((Int,Int,Int),(Int,Int,Int))]
parse = map ((\[x,y,z] -> ((x,y,z),(0,0,0))) . map read . (\[a,b,c,d,e,f,g] -> [b,d,f]) . groupBy ((==) `on` (`elem` "1234567890-"))) . lines

step :: [((Int,Int,Int),(Int,Int,Int))] -> [((Int,Int,Int),(Int,Int,Int))]
step ms =
    let gs = map (\(p,v) -> (p,) $ (v+) $ sum $ map (\(p',v') -> signum (p'-p)) ms) ms
        vs = map (\(p,v) -> (p+v,v)) gs
    in vs

instance Num (Int,Int,Int) where
    (x,y,z) + (a,b,c) = (x+a,y+b,z+c)
    (x,y,z) * (a,b,c) = (x*a,y*b,z*c)
    abs (x,y,z) = (abs x, abs y, abs z)
    signum (x,y,z) = (signum x, signum y, signum z)
    fromInteger n = (fromInteger n,fromInteger n,fromInteger n)
    negate (x,y,z) = (-x,-y,-z)

energy :: [((Int,Int,Int),(Int,Int,Int))] -> Int
energy = sum . map (\((a,b,c),(x,y,z)) -> (abs a+abs b+abs c)*(abs x+abs y+abs z))