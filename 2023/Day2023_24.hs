module Day2023_24 where

import Data.List.Split (splitOn)
import Data.Ratio (Ratio, (%), numerator)
import Data.Maybe (mapMaybe)
import Data.Matrix ( fromList, inverse, toList, toLists )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/24.txt"
    -- part 1
    print $ filterValid $ map parse $ lines contents
    -- part 2
    print $ numerator $ solvePosition $ map parse' $ lines contents

type Coords2 = ((Integer,Integer),(Integer,Integer))
type Sol = (Ratio Integer,Ratio Integer)
type Coords3 = ((Ratio Integer,Ratio Integer,Ratio Integer),(Ratio Integer,Ratio Integer,Ratio Integer))

parse :: String -> Coords2
parse = (\[[a,b,c],[d,e,f]] -> ((a,b),(d,e))) . map (map read . splitOn ",") . splitOn "@"

parse' :: String -> Coords3
parse' = (\[[a,b,c],[d,e,f]] -> ((a,b,c),(d,e,f))) . map (map (fromIntegral . read) . splitOn ",") . splitOn "@"

cr :: Num c => (c, c, c) -> (c, c, c) -> (c, c, c)
cr (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2,a3*b1-a1*b3,a1*b2-a2*b1)

dt :: Num a => (a, a, a) -> (a, a, a) -> a
dt (a,b,c) (d,e,f) = a*d + b*e + c*f

sub :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
sub (a,b,c) (d,e,f) = (a-d,b-e,c-f)

solvePosition :: [Coords3] -> Ratio Integer
solvePosition (((p1x,p1y,p1z),(v1x,v1y,v1z)):((p2x,p2y,p2z),(v2x,v2y,v2z)):((p3x,p3y,p3z),(v3x,v3y,v3z)):hs) =
    let c = [0      ,v1z-v2z,v2y-v1y,0      ,p2z-p1z,p1y-p2y,
             v2z-v1z,0      ,v1x-v2x,p1z-p2z,0      ,p2x-p1x,
             v1y-v2y,v2x-v1x,0      ,p2y-p1y,p1x-p2x,0      ,
             0      ,v3z-v2z,v2y-v3y,0      ,p2z-p3z,p3y-p2y,
             v2z-v3z,0      ,v3x-v2x,p3z-p2z,0      ,p2x-p3x,
             v3y-v2y,v2x-v3x,0      ,p2y-p3y,p3x-p2x,0      ]
        d = [p1z*v1y-p1y*v1z+p2y*v2z-p2z*v2y,
             p1x*v1z-p1z*v1x+p2z*v2x-p2x*v2z,
             p1y*v1x-p1x*v1y+p2x*v2y-p2y*v2x,
             p3z*v3y-p3y*v3z+p2y*v2z-p2z*v2y,
             p3x*v3z-p3z*v3x+p2z*v2x-p2x*v2z,
             p3y*v3x-p3x*v3y+p2x*v2y-p2y*v2x]
        Right c' = toLists <$> inverse (fromList 6 6 c)
    in negate $ sum $ take 3 $ map (sum . zipWith (*) d) c'

solve :: Coords2 -> Coords2 -> Maybe Sol
solve u@((a,b),(p,q)) v@((c,d),(r,s))
    | de == 0 = Nothing
    | otherwise =
        let x = (p*r*d - p*s*c - r*p*b + r*q*a)%(-de)
            y = (q*s*c - q*r*d - s*q*a + s*p*b)%de
        in Just (x,y)
    where de = s*p - q*r

solveWithTime :: Coords2 -> Coords2 -> Maybe Sol
solveWithTime u v = case solve u v of
    Just (x,y) -> if inFuture (x,y) u && inFuture (x,y) v then Just (x,y) else Nothing
    Nothing -> Nothing

solveWithTime' :: Coords2 -> Coords2 -> Maybe (Bool,Bool)
solveWithTime' u v = case solve u v of
    Just (x,y) -> Just (inFuture (x,y) u,inFuture (x,y) v)
    Nothing -> Nothing

inFuture :: Sol -> Coords2 -> Bool
inFuture (x,y) ((a,b),(p,q)) = signum (x-fromIntegral a) == signum (fromIntegral p) && signum (y-fromIntegral b) == signum (fromIntegral q)

inBounds :: (Ratio Integer,Ratio Integer) -> Sol -> Bool
inBounds (a,b) (x,y) = x >= a && x <= b && y >= a && y <= b

choose2 :: [a] -> [(a,a)]
choose2 (x:xs) = map (x,) xs ++ choose2 xs
choose2 [] = []

filterValid :: [Coords2] -> Int
filterValid = length . filter (inBounds (200000000000000,400000000000000)) . mapMaybe (uncurry solveWithTime) . choose2