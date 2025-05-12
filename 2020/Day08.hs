module Day08 where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/08.txt"
    let insts = HM.fromList $ zip [0..] $ parse $ lines contents
    let insts' = map (HM.fromList . zip [0..]) $ parse' $ lines contents
    -- part 1
    print $ run (0,0) insts
    -- part 2
    print $ head $ mapMaybe (run' (0,0)) insts'

parse :: [[Char]] -> [(Int, (Int, Int) -> (Int, Int))]
parse = map f
    where
        f line = case splitAt 4 line of
            ("jmp ",num) -> (0,\(c,n) -> (c+read (stripPlus num),n))
            ("acc ",num) -> (0,\(c,n) -> (c+1,n+read (stripPlus num)))
            ("nop ", _ ) -> (0,\(c,n) -> (c+1,n))
            where stripPlus cs@(c:rest) = if c == '+' then rest else cs

parse' :: [[Char]] -> [[(Int, (Int, Int) -> (Int, Int))]]
parse' (l:lines) = case splitAt 4 l of
    ("jmp ",num) -> ((0,\(c,n) -> (c+1,n)):parse lines):map ((0,\(c,n) -> (c+read (stripPlus num),n)):) (parse' lines)
    ("acc ",num) -> map ((0,\(c,n) -> (c+1,n+read (stripPlus num))):) (parse' lines) 
    ("nop ",num) -> ((0,\(c,n) -> (c+read (stripPlus num),n)):parse lines):map ((0,\(c,n) -> (c+1,n)):) (parse' lines)
    where stripPlus cs@(c:rest) = if c == '+' then rest else cs
parse' [] = []

run :: (Int, Int) -> HM.HashMap Int (Int, (Int, Int) -> (Int, Int)) -> (Int, Int)
run state insts
    | n == 1 = state
    | otherwise = run (f state) (HM.adjust (\(n',f') -> (n'+1,f')) (fst state) insts)
    where Just (n,f) = HM.lookup (fst state) insts

run' :: (Int, Int) -> HM.HashMap Int (Int, (Int, Int) -> (Int, Int)) -> Maybe Int
run' state insts
    | n == 1 = Nothing
    | n == -1 = Just (snd state)
    | otherwise = run' (f state) (HM.adjust (\(n',f') -> (n'+1,f')) (fst state) insts)
    where (n,f) = case HM.lookup (fst state) insts of
            Just (a,b) -> (a,b)
            Nothing -> (-1,id)