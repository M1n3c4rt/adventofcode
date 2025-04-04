import Utility.AOC ( neighbours8, enumerate, extrapolate )
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "11.txt"
    -- part 1
    print $ HM.size $ HM.filter (=='#') $ extrapolate (maxBound::Int) $ iterate step $ HM.fromList $ map (\(x,y,c) -> ((x,y),c)) $ enumerate contents
    -- part 2
    print $ HM.size $ HM.filter (=='#') $ extrapolate (maxBound::Int) $ iterate step' $ HM.fromList $ map (\(x,y,c) -> ((x,y),c)) $ enumerate contents

step :: HM.HashMap (Int, Int) Char -> HM.HashMap (Int, Int) Char
step grid = HM.mapWithKey helper grid
    where
        helper p v
            | v == 'L' && '#' `notElem` neighbours = '#'
            | v == '#' && length (filter (=='#') neighbours) >= 4 = 'L'
            | otherwise = v
            where neighbours = mapMaybe (`HM.lookup` grid) $ neighbours8 p

step' :: HM.HashMap (Int, Int) Char -> HM.HashMap (Int, Int) Char
step' grid = HM.mapWithKey helper grid
    where
        helper p@(a,b) v
            | v == 'L' && '#' `notElem` neighbours = '#'
            | v == '#' && length (filter (=='#') neighbours) >= 5 = 'L'
            | otherwise = v
            where
                neighbours = mapMaybe sightline $ neighbours8 p
                sightline (x,y)
                    | l == Just '.' = sightline (signum (x-a) + x, signum (y-b) + y)
                    | otherwise = l
                    where l = HM.lookup (x,y) grid