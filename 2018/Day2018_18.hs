module Day2018_18 where
import Utility.AOC (enumerateHM, neighbours8, extrapolate)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/18.txt"
    -- part 1
    print $ value $ (!!10) $ iterate step $ enumerateHM contents
    -- part 2
    print $ value $ extrapolate 1000000000 $ iterate step $ enumerateHM contents

step :: HM.HashMap (Int,Int) Char -> HM.HashMap (Int,Int) Char
step grid = HM.mapWithKey helper grid
    where
        helper p c = let s = mapMaybe (`HM.lookup` grid) $ neighbours8 p in case c of
            '.' -> if length (filter (=='|') s) >= 3 then '|' else '.'
            '|' -> if length (filter (=='#') s) >= 3 then '#' else '|'
            '#' -> if '#' `elem` s && '|' `elem` s then '#' else '.'

value :: HM.HashMap k Char -> Int
value grid = HM.size (HM.filter (=='|') grid) * HM.size (HM.filter (=='#') grid)