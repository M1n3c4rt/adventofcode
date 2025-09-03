module Day2015_06 where
import Utility.AOC (rangeIntersect, numbers)
import Data.Maybe (isNothing, mapMaybe)
import Debug.Trace (traceShow)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/06.txt"
    -- part 1
    --print $ S.size $ foldl helper S.empty $ lines contents
    -- part 2
    print $ sum $ HM.elems $ foldl helper' HM.empty $ lines contents

data Area = Area (Int,Int) (Int,Int) deriving Show

helper :: S.Set (Int,Int) -> String -> S.Set (Int,Int)
helper acc inst = case take 2 $ words inst of
    ["turn","on"] -> S.union (S.fromList [(x,y) | x <- [a..c], y <- [b..d]]) acc
    ["turn","off"] -> S.difference acc $ S.fromList [(x,y) | x <- [a..c], y <- [b..d]]
    ["toggle",_] -> foldr (\p acc' -> if p `S.member` acc then S.delete p acc' else S.insert p acc') acc [(x,y) | x <- [a..c], y <- [b..d]]
    where
        [a,b,c,d] = numbers inst

helper' :: HM.HashMap (Int,Int) Int -> String -> HM.HashMap (Int,Int) Int
helper' acc inst = case take 2 $ words inst of
    ["turn","on"] -> foldr (\p acc' -> HM.insertWith (+) p 1 acc') acc [(x,y) | x <- [a..c], y <- [b..d]]
    ["turn","off"] -> foldr (\p acc' -> HM.insertWith (\_ new -> if new == 0 then 0 else new-1) p 0 acc') acc [(x,y) | x <- [a..c], y <- [b..d]]
    ["toggle",_] -> foldr (\p acc' -> HM.insertWith (+) p 2 acc') acc [(x,y) | x <- [a..c], y <- [b..d]]
    where
        [a,b,c,d] = numbers inst
