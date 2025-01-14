import Data.List.Split (splitOn)
import Data.Ratio (Ratio, (%), numerator)
import Data.Maybe (mapMaybe)
import Data.Matrix ( fromList, inverse, toList, toLists )
import Debug.Trace ( trace )

main :: IO ()
main = do
    contents <- readFile "24.txt"
    -- part 1
    print $ filterValid $ map parse $ lines contents
    -- part 2
    print $ numerator $ solvePosition' $ map parse' $ lines contents

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
solvePosition ((p0,v0):(p1,v1):(p2,v2):hs) =
    let (c1,c2,c3) = (v0 `sub` v1) `cr` (p0 `sub` p1)
        (c4,c5,c6) = (v0 `sub` v2) `cr` (p0 `sub` p2)
        (c7,c8,c9) = (v1 `sub` v2) `cr` (p1 `sub` p2)
        d1 = (v0 `sub` v1) `dt` (p0 `cr` p1)
        d2 = (v0 `sub` v2) `dt` (p0 `cr` p2)
        d3 = (v1 `sub` v2) `dt` (p1 `cr` p2)
        Right [k1,k2,k3,k4,k5,k6,k7,k8,k9] = toList <$> inverse (fromList 3 3 [c1,c2,c3,c4,c5,c6,c7,c8,c9])
    in k1*d1 + k2*d2 + k3*d3 + k4*d1 + k5*d2 + k6*d3 + k7*d1 + k8*d2 + k9*d3

solvePosition' :: [Coords3] -> Ratio Integer
solvePosition' (((p1x,p1y,p1z),(v1x,v1y,v1z)):((p2x,p2y,p2z),(v2x,v2y,v2z)):((p3x,p3y,p3z),(v3x,v3y,v3z)):hs) =
    let c = [0      ,v1z-v2z,v2y-v1y,0      ,p2z-p1z,p1y-p2y,
             v2z-v1z,0      ,v1x-v2x,p1z-p2z,0      ,p2x-p1x,
             v1y-v2y,v2x-v1x,0      ,p2y-p1y,p1x-p2x,0      ,
             0      ,v3z-v2z,v2y-v3y,0      ,p2z-p3z,p3y-p2y,
             v2z-v3z,0      ,v3x-v2x,p3z-p2z,0      ,p2x-p3x,
             v3y-v2y,v2x-v3x,0      ,p2y-p3y,p3x-p2x,0      ]
        d = [p2y*v2z-p2z*v2y,p2z*v2x-p2x*v2z,p2x*v2y-p2y*v2x,p2y*v2z-p2z*v2y,p2z*v2x-p2x*v2z,p2x*v2y-p2y*v2x]
        Right c' = trace (show $ toLists <$> inverse (fromList 6 6 c)) (toLists <$> inverse (fromList 6 6 c))
    in sum $ take 3 $ map (sum . zipWith (*) d) c'

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