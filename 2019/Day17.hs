module Day17 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Char (chr, ord)
import Utility.AOC (enumerateFilter, neighbours4)
import qualified Data.Set as S
import Data.List (intersperse)
import Intcode ( parse, runC, modify, getOutput )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/17.txt"
    let (dir,bot,scaffolding) = enumerate $ map chr $ getOutput $ runC [] $ parse contents
    -- part 1
    print $ getIntersections scaffolding
    -- part 2 (chunking is done manually, sorry!)
    print $ last $ getOutput $ runC (map ord $ inputify $ chunk $ findPath 0 bot dir scaffolding) $ modify 0 2 $ parse contents

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

inputify :: [Char] -> String
inputify chunks = unlines $ map init [chunks,fa,fb,fc] ++ ["n"]

fa :: String
fb :: String
fc :: String
(fa,fb,fc) = ("R,8,L,4,R,4,R,10,R,8,","L,12,L,12,R,8,R,8,","R,10,R,4,R,4,")