module Day2021_25 where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2021/25.txt"
    let cl = lines contents
        bx = length $ head cl
        by = length cl
    -- part 1
    print $ getFirstStop 1 $ iterate (move bx by) $ parse $ enumerate cl
    -- part 2
    -- gg!

enumerate :: [[Char]] -> HM.HashMap (Int,Int) Char
enumerate l = let el = map (zip [0..]) l in HM.fromList $ map (\(a,b,c) -> ((b,a),c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

parse :: HM.HashMap (Int, Int) Char -> HM.HashMap (Int, Int) (Int, Int)
parse = HM.map helper . HM.filter (/='.')
    where
        helper c = case c of 
            '>' -> (1,0)
            'v' -> (0,1)

move :: Int -> Int -> HM.HashMap (Int, Int) (Int, Int) -> HM.HashMap (Int, Int) (Int, Int)
move bx by cs = HM.foldrWithKey (\k v a -> if v == (1,0) then HM.insert k v a else HM.insert (helper k v easts) v a) HM.empty easts
    where
        easts = HM.foldrWithKey (\k v a -> if v == (0,1) then HM.insert k v a else HM.insert (helper k v cs) v a) HM.empty cs
        helper (x,y) (p,q) m =
            let next = ((x+p) `mod` bx , (y+q) `mod` by)
            in
                case HM.lookup next m of 
                    Nothing -> next
                    Just _ -> (x,y)

getFirstStop :: Int -> [HM.HashMap (Int, Int) (Int, Int)] -> Int
getFirstStop n (a:b:cs)
    | a == b = n
    | otherwise = getFirstStop (n+1) (b:cs)