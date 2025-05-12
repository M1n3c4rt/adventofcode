module Day22 where

import Data.List.Split (splitOn)
import qualified Data.HashMap.Internal as HM
import Data.List (sortOn, nub)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/22.txt"
    let bricks = parse contents
    let movedBricks = moveDown (HM.fromList bricks) (sortOn (top . snd) bricks) HM.empty []
    let intmap = HM.map (bricksDirectlyBelow movedBricks) movedBricks
    -- part 1
    print $ safeBricks intmap
    -- part 2
    print $ sum $ map (subtract 1 . calcChain intmap . return) [0..length bricks]

type Brick = [(Int,Int,Int)]
type Bricks = HM.HashMap Int Brick

top :: Brick -> Int
top brick = maximum (map (\(a,b,c) -> c) brick)
bottom :: Brick -> Int
bottom brick = minimum (map (\(a,b,c) -> c) brick)

parse :: String -> [(Int,Brick)]
parse = zip [0..] . map (expand . map (map read . splitOn ",") . splitOn "~") . lines
    where expand [[a,b,c],[d,e,f]] = [(x,y,z) | x <- [a..d], y <- [b..e], z <- [c..f]]

bricksBelow :: Bricks -> Int -> [Brick]
bricksBelow bricks b' =
    let brick = HM.lookupDefault [(0,0,0)] b' bricks
        bot = bottom brick
        aligned qs ps = any (\(a,b,c) -> any (\(d,e,f) -> a == d && b == e) ps) qs
        below ps qs = top qs < bottom ps
        candidates = HM.filter (\v -> aligned brick v && below brick v) bricks
        helper c acc
            | tc > ta = [c]
            | tc == ta = c:acc
            | otherwise = acc
            where (tc,ta) = (top c, top $ head acc)
        pruned = HM.foldr helper [[(0,0,0)]] candidates in pruned

bricksDirectlyBelow :: Bricks -> Brick -> [Int]
bricksDirectlyBelow bricks brick =
    let bot = bottom brick
        aligned qs ps = any (\(a,b,c) -> any (\(d,e,f) -> a == d && b == e) ps) qs
        below ps qs = top qs == bottom ps - 1
        candidates = HM.filter (\v -> aligned brick v && below brick v) bricks in HM.keys candidates

moveDown :: Bricks -> [(Int,Brick)] -> Bricks -> [(Int,Brick)] -> Bricks
moveDown all ((i,brick):bricks) finished unfinished
    | below == [[(0,0,0)]] = let newbrick = helper b 0 brick in
        moveDown (HM.insert i newbrick all) bricks (HM.insert i newbrick finished) unfinished
    | any (`elem` finished) below =
        let t = top $ head below
            newbrick = helper b t brick
        in moveDown (HM.insert i newbrick all) bricks (HM.insert i newbrick finished) unfinished
    | otherwise = moveDown all bricks finished ((i,brick):unfinished)
    where below = bricksBelow all i
          b = bottom brick
          helper p q = map (\(a,b,c) -> (a,b,c-(p-q-1)))

moveDown all [] finished unfinished
    | null unfinished = finished
    | otherwise = moveDown all (sortOn (top . snd) unfinished) finished []

unsafeBricks :: HM.HashMap Int [Int] -> Int
unsafeBricks = length . nub . HM.foldr (\[c] acc -> c:acc) [] . HM.filter ((==1) . length)

calcChain :: HM.HashMap Int [Int] -> [Int] -> Int
calcChain ns unsafe =
    let newns = foldr HM.delete ns unsafe
        newUnsafe = HM.keys $ HM.filter (\l -> all (`elem` unsafe) l && not (null l)) newns in
        if null newUnsafe then length unsafe else calcChain newns (newUnsafe++unsafe)

safeBricks :: HM.HashMap Int [Int] -> Int
safeBricks intmap = HM.size intmap - unsafeBricks intmap