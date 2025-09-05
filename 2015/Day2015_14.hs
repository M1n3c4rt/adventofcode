module Day2015_14 where
import Utility.AOC (numbers)
import Data.List.Extra (sortOn, groupOn)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/14.txt"
    -- part 1
    print $ maximum $ map (\(a,b,c,d,e) -> a) $ (!!2503) $ iterate (map updateReindeer) $ map parse $ lines contents
    -- part 2
    print $ maximum $ HM.elems $ HM.fromListWith (+) $ concatMap lead $ take 2503 $ tail $ iterate (map updateReindeer) $ map parse $ lines contents

data State = Flying Int | Resting Int deriving Show
type Reindeer = (Int,Int,Int,Int,State)

updateReindeer :: Reindeer -> Reindeer
updateReindeer (total,flyPow,flyTime,rest,Flying 1) = (total+flyPow,flyPow,flyTime,rest,Resting rest)
updateReindeer (total,flyPow,flyTime,rest,Flying n) = (total+flyPow,flyPow,flyTime,rest,Flying (n-1))
updateReindeer (total,flyPow,flyTime,rest,Resting 1) = (total,flyPow,flyTime,rest,Flying flyTime)
updateReindeer (total,flyPow,flyTime,rest,Resting n) = (total,flyPow,flyTime,rest,Resting (n-1))

parse l = let [a,b,c] = numbers l in (0,a,b,c,Flying b)

lead :: [Reindeer] -> [((Int,Int,Int),Int)]
lead = map (\(a,b,c,d,e) -> ((b,c,d),1)) . last . groupOn (\(a,b,c,d,e) -> a) . sortOn (\(a,b,c,d,e) -> a)