module Day2019_22 where
import Data.Ratio (Ratio, (%), denominator, numerator)
import Utility.AOC (extrapolate)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/22.txt"
    let p = 10007
        p' = 119315717514047
        n = 101741582076661
    -- part 1
    print $ (\(c,m) -> (m*2019+c) `mod` p) $ foldr (compose p . normal . parse) (0,1) $ reverse $ lines contents
    -- part 2
    print $ (\(c,m) -> (m*2020+c) `mod` p') $ ex p' n $ foldr (compose p' . inverted p' . parse) (0,1) $ lines contents

parse :: String -> (String,Integer)
parse line
    | take 6 line == "deal i" = ("R",0)
    | take 6 line == "deal w" = let k = read (drop 20 line) in ("J",k)
    | otherwise = let k = read (drop 4 line) in ("S",k)

normal :: Num b => (String, b) -> (b, b)
normal f = case f of
    ("R",_) -> (-1,-1)
    ("S",k) -> (-k,1)
    ("J",k) -> (0,k)

inverted :: Integral b => b -> (String, b) -> (b, b)
inverted p f = case f of
    ("R",_) -> (-1,-1)
    ("S",k) -> (k,1)
    ("J",k) -> (0,computeM 1 k)
    where computeM mult k = let r = ceiling (mult*p%k) in if (k * r) `mod` p == 1 then r else computeM (mult+1) k

compose :: Integral b => b -> (b, b) -> (b, b) -> (b, b)
compose p (c,m) (c',m') = ((m*c'+c) `mod` p,(m*m') `mod` p)

ex :: (Integral b, Integral a) => b -> a -> (b, b) -> (b, b)
ex p 0 f = (0,1)
ex p n f = let f' = ex p (n `div` 2) f in if even n then compose p f' f' else compose p f (compose p f' f')