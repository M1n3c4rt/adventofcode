module Day2018_03 where
import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Function (on)
import Utility.AOC (rangeIntersect)
import Data.Maybe (isJust, isNothing, mapMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/03.txt"
    let claims = map parse $ lines contents
    -- part 1
    print $ sum $ map area $ foldr (\c acc -> c : concatMap (`claimSubtract` c) acc) [] $ mapMaybe (\[x,y] -> claimIntersect x y) $ choose 2 claims
    -- part 2
    print $ cid $ head $ filter (\c -> (==1) $ length $ filter (isJust . claimIntersect c) claims) claims

data Claim = Claim {cid :: Int, start :: (Int,Int), end :: (Int,Int)} deriving Show

parse :: String -> Claim
parse = (\[a,b,c,d,e] -> Claim { cid = a, start = (b, c), end = (b+d,c+e) }) . map (read . snd) . filter (even . fst) . zip [1..] . groupBy ((==) `on` isDigit)

area :: Claim -> Int
area (Claim n (a,b) (c,d)) = (c-a)*(d-b)

claimIntersect :: Claim -> Claim -> Maybe Claim
claimIntersect c1 c2 =
    let (a,b) = start c1
        (c,d) = end c1
        (e,f) = start c2
        (g,h) = end c2
        candidate = liftA2 (,) (rangeIntersect (a,c-1) (e,g-1)) (rangeIntersect (b,d-1) (f,h-1))
    in uncurry (Claim 0) . (\((x,y),(z,w)) -> ((x,z),(y+1,w+1))) <$> candidate

claimSubtract :: Claim -> Claim -> [Claim]
claimSubtract c1 c2 =
    let (a,b) = start c1
        (c,d) = end c1
        (p,q) = start c2
        (r,s) = end c2
        ((e,f),(g,h)) = ((max a p, max b q),(min c r, min d s))
        isEmpty ((x,y),(z,w)) = x >= z || y >= w
    in if isNothing (claimIntersect c1 c2) then [c1] else map (uncurry (Claim 0)) $ filter (not . isEmpty) [
        ((a,b),(c,f)),
        ((a,f),(e,h)),
        ((g,f),(c,h)),
        ((a,h),(c,d))
    ]

choose :: (Num n, Eq n) => n -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs