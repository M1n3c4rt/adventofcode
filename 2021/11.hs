import qualified Data.HashMap.Strict as HM
import Data.Char (chr)

main :: IO ()
main = do
    contents <- readFile "11.txt"
    -- part 1
    print $ sum $ map getFlashes $ take 101 $ iterate update $ enumerate $ lines contents
    -- part 2
    print $ fst $ head $ dropWhile ((/=100) . snd) $ zipWith (\a b -> (a, getFlashes b)) [0..] $ iterate update $ enumerate $ lines contents

enumerate :: [[Char]] -> HM.HashMap (Int,Int) Int
enumerate l = let el = map (zip [0..]) l in HM.fromList $ map (\(a,b,c) -> ((a,b),read [c])) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x+p,y+q) | p <- [-1,0,1], q <- [-1,0,1], p /= 0 || q /= 0]

update :: HM.HashMap (Int,Int) Int -> HM.HashMap (Int,Int) Int
update os =
    let next = flash $ HM.map (+1) os
        flash os'
            | all (<=9) os' = os'
            | otherwise =
                let flashed = HM.map (\n -> if n > 9 then -1 else n) os'
                    helper p k m = if k == -1 then 0 else length $ filter (\n -> HM.lookupDefault 0 n m == -1) $ neighbours p
                    inc = HM.mapWithKey (\p k -> k + helper p k flashed - helper p k os') flashed
                in flash inc
    in HM.map (`max` 0) next

pprint :: Int -> Int -> HM.HashMap (Int,Int) Int -> String
pprint h w os = unlines [[chr (k+48) | y <- [0..h-1], let k = HM.lookupDefault 0 (x,y) os] | x <- [0..w-1]]

getFlashes :: HM.HashMap (Int,Int) Int -> Int
getFlashes = length . filter (==0) . HM.elems

