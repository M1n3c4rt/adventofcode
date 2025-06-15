module Day2018_23 where
import Data.Foldable (maximumBy)
import Data.Function (on)
import Utility.AOC (taxicab3, rangeIntersect, neighbours6)
import Data.List.Split (splitOn)
import Control.Monad (zipWithM)
import Data.Maybe (fromMaybe)
import System.Random.Shuffle (shuffleM, shuffle')
import System.Random (newStdGen)
import Data.List (nub, sort, group, sortOn)
import Data.Ratio ( (%), denominator, numerator )
import Data.Matrix ( fromList, toList )
import Data.Bifunctor (bimap)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/23.txt"
    let bots = map parse $ lines contents
        l = length bots
        (strongP,strongR) =  maximumBy (compare `on` snd) bots
    -- part 1
    print $ length $ filter ((\p -> taxicab3 strongP p <= strongR) . fst) bots
    -- part 2
    cs <- head . last . sortOn length . group . sort <$> shuffle 100 (map botToOctahedron bots) l
    print $ minimum $ map sum $ reduceOctahedron cs

shuffle :: Int -> [Octahedron] -> Int -> IO [Octahedron]
shuffle count octs l = do
    gen <- newStdGen
    let new = last $ scanl1 (\acc c -> fromMaybe acc (intersectOctahedra c acc)) $ shuffle' octs l gen
    print new
    if count == 0 then return [] else (new:) <$> shuffle (count-1) octs l

type Bot = ((Int,Int,Int),Int)
type Octahedron = [(Int,Int)]

parse :: String -> Bot
parse = (\[a,b] -> (read $ "(" ++ drop 5 (init a) ++ ")",read $ drop 2 b)) . splitOn ", "

inRange :: Bot -> (Int, Int, Int) -> Bool
inRange (p,r) q = taxicab3 p q <= r

botToOctahedron :: Bot -> Octahedron
botToOctahedron ((a,b,c),r) = [
        (a+b+c-r,a+b+c+r), -- - x - y - z <= r - a - b - c, + x + y + z <= r + a + b + c  
        (a+b-c-r,a+b-c+r), -- - x - y + z <= r - a - b + c, + x + y - z <= r + a + b - c 
        (a-b+c-r,a-b+c+r), -- - x + y - z <= r - a + b - c, + x - y + z <= r + a - b + c 
        (a-b-c-r,a-b-c+r)  -- - x + y + z <= r - a + b + c, + x - y - z <= r + a - b - c 
    ]

intersectOctahedra :: Octahedron -> Octahedron -> Maybe Octahedron
intersectOctahedra = zipWithM rangeIntersect

reduceOctahedron :: Octahedron -> [[Int]]
reduceOctahedron o = filter (\[x,y,z] -> (x-y-z) `range` (g,h)) $ map (map numerator) $ filter (all ((==1) . denominator)) $ [(toList . (m*) . fromList 3 1) [x,y,z] | x <- [a..b], y <- [c..d], z <- [e..f]]
    where
        [(a,b),(c,d),(e,f),(g,h)] = map (bimap fromIntegral fromIntegral) o
        m = fromList 3 3 [0 % 1,1 % 2,1 % 2,1 % 2,0 % 1,(-1) % 2,1 % 2,(-1) % 2,0 % 1]
        x `range` (p,q) = x >= floor p && x <= floor q