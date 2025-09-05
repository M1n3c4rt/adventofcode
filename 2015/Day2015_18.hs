module Day2015_18 where
import Utility.AOC (enumerateHM, neighbours8)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/18.txt"
    -- part 1
    print $ HM.size $ HM.filter id $ (!!100) $ iterate step $ HM.map (=='#') $ enumerateHM contents
    -- part 2
    print $ HM.size $ HM.filter id $ (!!100) $ iterate step' $ corners $ HM.map (=='#') $ enumerateHM contents

step :: HM.HashMap (Int,Int) Bool -> HM.HashMap (Int,Int) Bool
step m = HM.mapWithKey (\k v -> let ns = length $ filter id $ mapMaybe (`HM.lookup` m) $ neighbours8 k in if v then ns `elem` [2,3] else ns == 3) m

step' :: HM.HashMap (Int,Int) Bool -> HM.HashMap (Int,Int) Bool
step' m = HM.mapWithKey (\k v -> (k `elem` [(0,0),(99,0),(0,99),(99,99)]) || (let ns = length $ filter id $ mapMaybe (`HM.lookup` m) $ neighbours8 k in if v then ns `elem` [2,3] else ns == 3)) m

corners :: HM.HashMap (Int,Int) Bool -> HM.HashMap (Int,Int) Bool
corners m = foldr (uncurry HM.insert) m [((0,0),True),((99,0),True),((0,99),True),((99,99),True)]