module Day23 where

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/23.txt"
    -- part 1
    print $ ground $ (!!9) $ move 0 $ elves $ lines contents
    -- part 2
    print $ (+1) $ length $ move 0 $ elves $ lines contents

elves :: [[Char]] -> S.Set (Int, Int)
elves l = let el = map (zip [0..]) l in S.fromList $ map (\(a,b,c) -> (a,b)) $ filter (\(a,b,c) -> c == '#') $ map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

propose :: S.Set (Int,Int) -> Int -> (Int,Int) -> (Int,Int)
propose elves offset (x,y) =
    let neighbours = [(x,y-1),(x+1,y-1),(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1)]
        takeFirst ls = case ls of
            [] -> (x,y)
            [a,b,c]:xs -> neighbours !! b
        proposals = filter (not . any ((`S.member` elves) . (neighbours !!))) $ take 4 $ drop offset $ cycle [[7,0,1],[3,4,5],[5,6,7],[1,2,3]]
    in if length proposals == 4 then (x,y) else takeFirst proposals

move :: Int -> S.Set (Int, Int) -> [S.Set (Int, Int)]
move offset elves =
    let proposals = foldr (uncurry HM.insert) HM.empty $ S.map (\e -> (e,propose elves offset e)) elves
        accepted = HM.filterWithKey (/=) $ HM.filter (\v -> (==1) $ HM.size $ HM.filter (==v) proposals) proposals
        helper e = fromMaybe e (HM.lookup e accepted)
        elves' = S.map helper elves
    in if HM.size accepted == 0 then [] else elves':move ((offset+1) `mod` 4) elves'

ground :: S.Set (Int, Int) -> Int
ground s =
    let fsts = S.map fst s
        snds = S.map snd s
        a = minimum fsts
        b = minimum snds
        c = maximum fsts
        d = maximum snds
        range = S.fromList [(x,y) | x <- [a..c], y <- [b..d]]
    in S.size $ S.difference range s