module Day15 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (Down(Down))
import Data.List (sortOn)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/15.txt"
    let u@(a,b,c,d,e,f) = advance S.empty [(False,(0,0),0,0,0,parse contents)]
    -- part 1
    print (b, c)
    -- part 2
    print $ (\(a1,a2,a3,a4,a5,a6) -> a3) $ advance S.empty [(False,b,0,d,e,f)]

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

run :: [Int] -> Int -> Int -> HM.HashMap Int Int -> (Int,Int,Int,HM.HashMap Int Int)
run inputs pointer relBase state = case inst of
    1 -> run inputs (pointer+4) relBase $ HM.insert cW (a+b) state
    2 -> run inputs (pointer+4) relBase $ HM.insert cW (a*b) state
    3 -> run (tail inputs) (pointer+2) relBase $ HM.insert aW (head inputs) state
    4 -> (a,pointer+2,relBase,state)
    5 -> run inputs (if a == 0 then pointer+3 else b) relBase state
    6 -> run inputs (if a /= 0 then pointer+3 else b) relBase state
    7 -> run inputs (pointer+4) relBase $ HM.insert cW (if a < b then 1 else 0) state
    8 -> run inputs (pointer+4) relBase $ HM.insert cW (if a == b then 1 else 0) state
    9 -> run inputs (pointer+2) (relBase+a) state
    where
        (inst:mods) = splitMode $ HM.lookupDefault 0 pointer state

        vals@(a':b':c':vs) = map (\x -> HM.lookupDefault 0 (pointer+x) state) [1..]
        refs@(a :b :c :rs) = zipWith applyMod vals mods
        refWrites@(aW:bW:cW:rws) = zipWith applyModW vals mods

        applyMod x' m = case m of
            0 -> HM.lookupDefault 0 x' state
            1 -> x'
            2 -> HM.lookupDefault 0 (relBase + x') state

        applyModW x' m = case m of
            0 -> x'
            2 -> relBase + x'

splitMode :: (Integral a, Read a, Show a) => a -> [a]
splitMode n = n `mod` 100 : map (read . pure) (reverse $ show $ n `div` 100) ++ repeat 0

advance :: S.Set (Int,Int) -> [(Bool, (Int,Int), Int, Int, Int, HM.HashMap Int Int)] -> (Bool, (Int,Int), Int, Int, Int, HM.HashMap Int Int)
advance visited (u@(isTank,(a,b),n,pointer,relBase,state):unexplored)
    | isTank || (null unexplored && null filtered) = u
    | otherwise = advance (S.insert (a,b) visited) $ unexplored ++ filtered
    where
        neighbours = zipWith (\x point -> (point,) $ run [x] pointer relBase state) [1,2,3,4] [(a,b+1),(a,b-1),(a+1,b),(a-1,b)]
        helper (point,(m,p,r,s)) = case m of
            0 -> Nothing
            1 -> if point `S.member` visited then Nothing else Just (False,point,n+1,p,r,s)
            2 -> if point `S.member` visited then Nothing else Just (True,point,n+1,p,r,s)
        filtered = mapMaybe helper neighbours
