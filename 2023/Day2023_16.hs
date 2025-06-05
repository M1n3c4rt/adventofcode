module Day2023_16 where

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/16.txt"
    let stepmap = HM.unions $ map generateNode $ enumerate $ lines contents
    -- part 1
    print $ energize S.empty (S.singleton (0,0,1,0)) stepmap
    -- part 2
    print $ maximum $ map (\n -> energize S.empty n stepmap) bounds

type Node = (Int,Int,Int,Int)

inBounds :: (Ord a, Num a) => (a, a) -> Bool
inBounds (a,b) = min a b >= 0 && max a b < 110

bounds :: [S.Set Node]
bounds = map S.singleton $ concat [n,e,w,s]
    where n = map (,0,0,1) [0..109]
          e = map (109,,-1,0) [0..109]
          w = map (0,,1,0) [0..109]
          s = map (,109,0,-1) [0..109]

energize :: S.Set Node -> S.Set Node -> HM.HashMap Node (S.Set Node) -> Int
energize finished frontier stepmap
    | S.null frontier = S.size $ S.map (\(x,y,_,_) -> (x,y)) finished
    | otherwise = energize newFinished newFrontier stepmap
    where newFinished = S.union finished frontier
          unfilteredFrontier = S.unions $ S.map (\t -> HM.lookupDefault S.empty t stepmap) frontier
          newFrontier = S.filter (\x@(a,b,c,d) -> inBounds (a,b) && S.notMember x finished) unfilteredFrontier

enumerate :: [[a]] -> [(Int,Int,a)]
enumerate l = let el = map (zip [0..]) l in map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

generateNode :: (Int,Int,Char) -> HM.HashMap Node (S.Set Node)
generateNode n@(x,y,c) = case c of
    '.' -> toMap [((0,1),[(0,1)]) , ((0,-1),[(0,-1)]) , ((1,0),[(1,0)]) , ((-1,0),[(-1,0)])]
    '/' -> toMap [((0,1),[(-1,0)]) , ((0,-1),[(1,0)]) , ((1,0),[(0,-1)]) , ((-1,0),[(0,1)])]
    '\\' -> toMap [((0,1),[(1,0)]) , ((0,-1),[(-1,0)]) , ((1,0),[(0,1)]) , ((-1,0),[(0,-1)])]
    '|' -> toMap [((0,1),[(0,1)]) , ((0,-1),[(0,-1)]) , ((1,0),[(0,1),(0,-1)]) , ((-1,0),[(0,-1),(0,1)])]
    '-' -> toMap [((0,1),[(1,0),(-1,0)]) , ((0,-1),[(-1,0),(1,0)]) , ((1,0),[(1,0)]) , ((-1,0),[(-1,0)])]
    where toMap = HM.fromList . map (\((p,q),ls) -> ((x,y,p,q), S.fromList $ map (\(a,b) -> (x+a,y+b,a,b)) ls))