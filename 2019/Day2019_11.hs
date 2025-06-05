module Day2019_11 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Intcode 

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/11.txt"
    let (updated,tiles) = runTiles S.empty S.empty (0,0) (0,1) $ parse contents
    let (updated',tiles') = runTiles S.empty (S.singleton (0,0)) (0,0) (0,1) $ parse contents
    -- part 1
    print $ S.size updated
    -- part 2
    putStr $ pprint tiles'

runTiles :: S.Set (Int, Int) -> S.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> CompilerState -> (S.Set (Int, Int), S.Set (Int, Int))
runTiles painted white pos dir state = if isFinished state then (painted,white) else
    let newState = runC [if pos `S.member` white then 1 else 0] state
        [a,b] = getOutput newState
        (newPos,newDir) = move pos dir b
        newPainted = S.insert pos painted
        newWhite = if a == 0 then S.delete pos white else S.insert pos white
    in runTiles newPainted newWhite newPos newDir newState

move :: (Int, Int) -> (Int, Int) -> Int -> ((Int, Int), (Int, Int))
move (x,y) (p,q) a = case a of
    0 -> ((x-q,y+p),(-q,p))
    1 -> ((x+q,y-p),(q,-p))

pprint :: S.Set (Int,Int) -> String
pprint points = unlines [concat [if (x,y) `S.member` points then "##" else "  " | x <- [xmin..xmax]] | y <- reverse [ymin..ymax]]
    where 
        xs = S.map fst points
        ys = S.map snd points
        (xmin,xmax,ymin,ymax) = (minimum xs, maximum xs, minimum ys, maximum ys)