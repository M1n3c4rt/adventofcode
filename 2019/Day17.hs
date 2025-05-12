module Day17 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Char (chr, ord)
import Utility.AOC (enumerateFilter, neighbours4)
import qualified Data.Set as S
import Data.List (intersperse)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/17.txt"
    let (dir,bot,scaffolding) = enumerate $ map chr $ run [] 0 0 $ parse contents
    -- part 1
    print $ getIntersections scaffolding
    -- part 2 (chunking is done manually, sorry!)
    print $ last $ run (map ord $ inputify $ chunk $ findPath 0 bot dir scaffolding) 0 0 $ HM.insert 0 2 $ parse contents

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

run :: [Int] -> Int -> Int -> HM.HashMap Int Int -> [Int]
run inputs pointer relBase state = case inst of
    1 -> run inputs (pointer+4) relBase $ HM.insert cW (a+b) state
    2 -> run inputs (pointer+4) relBase $ HM.insert cW (a*b) state
    3 -> run (tail inputs) (pointer+2) relBase $ HM.insert aW (head inputs) state
    4 -> a : run inputs (pointer+2) relBase state
    5 -> run inputs (if a == 0 then pointer+3 else b) relBase state
    6 -> run inputs (if a /= 0 then pointer+3 else b) relBase state
    7 -> run inputs (pointer+4) relBase $ HM.insert cW (if a < b then 1 else 0) state
    8 -> run inputs (pointer+4) relBase $ HM.insert cW (if a == b then 1 else 0) state
    9 -> run inputs (pointer+2) (relBase+a) state
    99 -> []
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

enumerate :: String -> ((Int, Int),(Int, Int), S.Set (Int, Int))
enumerate cs =
    let dir
            | '>' `elem` cs = (1,0)
            | 'v' `elem` cs = (0,1)
            | '<' `elem` cs = (-1,0)
            | '^' `elem` cs = (0,-1)
        bot = head $ enumerateFilter (`elem` ">^<v") cs
        tiles = S.fromList $ (bot:) $ enumerateFilter (=='#') cs
    in (dir,bot,tiles)

getIntersections :: S.Set (Int,Int) -> Int
getIntersections s = sum $ map (uncurry (*)) $ S.toList $ S.filter ((==4) . helper) s
    where helper p = length $ filter (`elem` s) $ neighbours4 p

findPath :: Int -> (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> [Char]
findPath steps (x,y) (p,q) scaffolding
    | (x+p,y+q) `S.member` scaffolding = findPath (steps+1) (x+p,y+q) (p,q) scaffolding
    | (x-q,y+p) `S.member` scaffolding = appender $ ("R,"++) $ findPath 0 (x,y) (-q,p) scaffolding
    | (x+q,y-p) `S.member` scaffolding = appender $ ("L,"++) $ findPath 0 (x,y) (q,-p) scaffolding
    | otherwise = appender []
    where appender
            | steps == 0 = id
            | otherwise = ((show steps++",")++)

chunk :: [Char] -> [Char]
chunk insts
    | take (length fa) insts == fa = "A," ++ chunk (drop (length fa) insts)
    | take (length fb) insts == fb = "B," ++ chunk (drop (length fb) insts)
    | take (length fc) insts == fc = "C," ++ chunk (drop (length fc) insts)
chunk [] = []

inputify chunks = unlines $ map init [chunks,fa,fb,fc] ++ ["n"]

fa :: String
fb :: String
fc :: String
(fa,fb,fc) = ("R,8,L,4,R,4,R,10,R,8,","L,12,L,12,R,8,R,8,","R,10,R,4,R,4,")