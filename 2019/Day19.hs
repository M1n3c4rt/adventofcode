module Day19 where
import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId, traceShow, trace)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/19.txt"
    -- part 1
    --print $ HM.size $ HM.filter id $ grid 49 $ parse contents
    -- part 2
    print $ initFindSquare $ parse contents
    --print $ map (\(x,y) -> (x,-y)) $ HM.keys $ HM.filter id $ grid 151 $ parse contents

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

grid :: Int -> HM.HashMap Int Int -> HM.HashMap (Int, Int) Bool
grid n state = HM.fromList [((x,y),run [x,y] 0 0 state==[1]) | x <- [0..n], y <- [0..n]]

pprint :: Int -> HM.HashMap (Int, Int) Bool -> String
pprint n g = unlines [[if fromJust (HM.lookup (x,y) g) then '#' else '.' | x <- [0..n]] | y <- [0..n]]

findSquare :: (Int, Int) -> (Int, Int) -> HM.HashMap Int Int -> Int
findSquare (a,b) (c,d) state
    | c - a == 99 && b - d == 99 = traceShow ((a,b),(c,d)) $ 10000 * a + d
    | otherwise =
        let (a',b') = (a,b+1)
            (c',d') = (c+1,d)

            southWest = map (\(x,y) -> ((x,y),run [x,y] 0 0 state)) . iterate (\(x,y) -> (x-1,y+1))
            northEast = map (\(x,y) -> ((x,y),run [x,y] 0 0 state)) . iterate (\(x,y) -> (x+1,y-1))

            (newa,newb) = case run [a',b'] 0 0 state of
                [1] -> fst $ last $ takeWhile ((==[1]) . snd) $ southWest (a',b')
                [0] -> fst $ head $ dropWhile ((==[0]) . snd) $ northEast (a',b')
            (newc,newd) = case run [c',d'] 0 0 state of
                [1] -> fst $ last $ takeWhile ((==[1]) . snd) $ northEast (c',d')
                [0] -> fst $ head $ dropWhile ((==[0]) . snd) $ southWest (c',d')
        in traceShow ((a,b),(c,d)) $ findSquare (newa,newb) (newc,newd) state

initFindSquare :: HM.HashMap Int Int -> Int
initFindSquare state =
    let (a,b) = traceShowId $ head $ dropWhile (\(x,y) -> run [x,y] 0 0 state == [0]) $ iterate (\(x,y) -> (x-1,y+1)) (75,0)
        (c,d) = traceShowId $ head $ dropWhile (\(x,y) -> run [x,y] 0 0 state == [0]) $ iterate (\(x,y) -> (x+1,y-1)) (0,75)
    in findSquare (a,b) (c,d) state