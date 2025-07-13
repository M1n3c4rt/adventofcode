module Day2017_19 where
import Utility.AOC (enumerateHM, neighbours4)
import qualified Data.HashMap.Strict as HM
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId, traceShow)
import Data.Char (isAlpha)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/19.txt"
    let m = HM.filter (/=' ') $ enumerateHM contents
    -- part 1
    putStrLn $ filter isAlpha $ map snd $ (\(p,c) -> next (p,(0,1),c) m) $ findTop m
    -- part 2
    print $ length $ map snd $ (\(p,c) -> next (p,(0,1),c) m) $ findTop m

findTop :: HM.HashMap (Int,Int) Char -> ((Int,Int), Char)
findTop m = let p = minimumBy (compare `on` snd) $ HM.keys m in (p,HM.lookupDefault ' ' p m)

next (p,d,c) m = let ns = filter ((/=backward) . fst) $ mapMaybe (\x -> (x,) <$> HM.lookup x m) $ neighbours4 p in case ns of
    [(p',c')] -> (p,c) : next (p', sub p' p, c') m
    xs -> case HM.lookup forward m of
        Just c' -> (p,c) : next (forward,d,c') m
        Nothing -> [(p,c)]
    where
        sub (a,b) (x,y) = (a-x,b-y)
        forward = let (a,b) = p; (x,y) = d in (a+x,b+y)
        backward = let (a,b) = p; (x,y) = d in (a-x,b-y)

