module Day10 where

import Utility.AOC
import qualified Data.Set as S
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (sortOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/10.txt"
    let as = S.fromList $ enumerateFilter (=='#') contents
    let (p,vs) = maximumBy (compare `on` (S.size . snd)) $ S.map (\x -> (x,visible as x)) as
    -- part 1
    print $ S.size vs
    -- part 2
    print $ (\(x,y) -> 100*x+y) $ (!!199) $ sortOn (clockAngle p) $ S.toList vs

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween (x,y) (a,b) =
    let k = gcd (a-x) (b-y)
        (p,q) = ((a-x)`div`k,(b-y)`div`k)
    in if (x,y) == (a,b) then repeat (x,y) else
        takeWhile (/=(a,b)) $ tail $ iterate (\(m,n) -> (m+p,n+q)) (x,y)

visible :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
visible asteroids station =
    S.filter (not . any (`S.member` asteroids) . pointsBetween station) asteroids

clockAngle :: (Int,Int) -> (Int,Int) -> Double
clockAngle (x',y') (a,b)
    | x == 0 && y < 0 = 0
    | x > 0 && y < 0 = atan (p/q)
    | x > 0 && y == 0 = pi/2
    | x > 0 && y > 0 = pi/2 + atan (q/p)
    | x == 0 && y > 0 = pi
    | x < 0 && y > 0 = pi + atan (p/q)
    | x < 0 && y == 0 = 3*pi/2
    | x < 0 && y < 0 = 3*pi/2 + atan (q/p)
    | otherwise = 1/0
    where
        x = a-x'
        y = b-y'
        p = abs $ fromIntegral x
        q = abs $ fromIntegral y