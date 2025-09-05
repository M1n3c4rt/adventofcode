module Day2022_05 where

import Data.List (transpose)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/05.txt"
    let (grid,insts) = parse contents
    -- part 1
    putStrLn $ map head $ HM.elems $ foldl applyInst grid insts
    -- part 2
    putStrLn $ map head $ HM.elems $ foldl applyInst' grid insts

parse :: String -> (HM.HashMap Int String,[(Int,Int,Int)])
parse contents =
    let l = lines contents
        (a,b) = (take 8 l, drop 10 l)
        grid = HM.fromList $ zip [1..] $ map (dropWhile (==' ')) $ transpose $ map takeFours a
        takeFours [x,y,z] = [y]
        takeFours (x:y:z:d:ds) = y:takeFours ds
        insts = map (\s -> (read $ takeWhile (/=' ') $ drop 5 s, read [reverse s !! 5], read [last s])) b
    in (grid,insts)

applyInst :: HM.HashMap Int String -> (Int,Int,Int) -> HM.HashMap Int String
applyInst grid (a,b,c) =
    let removed = HM.adjust (drop a) b grid
        chunk = take a $ HM.lookupDefault [] b grid
        added = HM.adjust (reverse chunk ++) c removed
    in added

applyInst' :: HM.HashMap Int String -> (Int,Int,Int) -> HM.HashMap Int String
applyInst' grid (a,b,c) =
    let removed = HM.adjust (drop a) b grid
        chunk = take a $ HM.lookupDefault [] b grid
        added = HM.adjust (chunk ++) c removed
    in added