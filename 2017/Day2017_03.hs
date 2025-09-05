module Day2017_03 where
import Data.Maybe (mapMaybe)
import Utility.AOC (neighbours8)
import qualified Data.HashMap.Lazy as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/03.txt"
    -- part 1
    print $ fst $ parts $ abs $ grambulate $ read contents
    -- part 2
    let grid = map head $ tail $ map fst $ scanl (\(l,acc) c -> let (a,b) = insertAt acc c in (a:l,b)) ([],HM.singleton (0,0) 1) [2..]
    print $ head $ dropWhile (<read contents) grid

data Complex a = a :+ a deriving Show
infix 5 :+

grambulate :: Integer -> Complex Integer
grambulate 1 = 0:+0
grambulate n = if odd s then
        ((s`div`2):+(-(s `div` 2))) + (if d <= s then 1:+d-1 else (1:+s) + ((s-d+1):+0))
    else
        ((-(s`div`2)+1):+(s`div`2)) + (if d <= s then -(1:+d-1) else -((1:+s) + ((s-d+1):+0)))
    where
        s = last $ takeWhile ((<n) . (^2)) [1..]
        d = n-s^2

instance Num a => Num (Complex a) where
    (a:+b)+(c:+d) = a+c:+b+d
    (a:+b)*(c:+d) = a*c-b*d:+b*c+a*d
    negate (a:+b) = (-a):+(-b)
    signum (a:+b) = signum a :+ signum b
    abs (a:+b) = abs a + abs b :+ 0
    fromInteger n = fromInteger n :+ 0

parts (a:+b) = (a,b)

insertAt :: HM.HashMap (Integer, Integer) Integer -> Integer -> (Integer,HM.HashMap (Integer, Integer) Integer)
insertAt grid n = (dat,HM.insert c dat grid)
    where
        c = parts $ grambulate n
        dat = sum $ mapMaybe (`HM.lookup` grid) $ neighbours8 c