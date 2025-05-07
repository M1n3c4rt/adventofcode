import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId, traceShow, trace)
import System.Random ( Random(randoms), newStdGen )

main :: IO ()
main = do
    contents <- readFile "13.txt"
    let out = run [] 0 0 $ parse contents
    let outP = run (repeat 0) 0 0 $ HM.insert 0 2 $ cheat $ parse contents
    -- part 1
    print $ length $ filter (==2) $ takeEvery 3 out
    -- part 2
    putStr $ pprints HM.empty $ splitOn [99] outP

takeEvery :: Int -> [a] -> [a]
takeEvery n ns = case drop (n-1) ns of
    b:bs -> b:takeEvery n bs
    [] -> []

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

run :: [Int] -> Int -> Int -> HM.HashMap Int Int -> [Int]
run inputs pointer relBase state = case inst of
    1 -> run inputs (pointer+4) relBase $ HM.insert cW (a+b) state
    2 -> run inputs (pointer+4) relBase $ HM.insert cW (a*b) state
    3 -> (99:) $ run (tail inputs) (pointer+2) relBase $ HM.insert aW (head inputs) state
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

pprint :: HM.HashMap (Int,Int) Int -> [Int] -> (HM.HashMap (Int,Int) Int,String)
pprint prev out =
    let coords = helper out
        helper (x:y:z:zs) = HM.insert (x,y) z $ helper zs
        helper [] = prev
        charMap = HM.fromList [(0,' '),(1,'|'),(2,'#'),(3,'-'),(4,'O')]
        [(minx,miny),(maxx,maxy)] = map ($ HM.keys coords) [minimum,maximum]
    in (coords,"Score: " ++ show (HM.lookupDefault 0 (-1,0) coords) ++ "\n" ++ unlines [[HM.lookupDefault ' ' (HM.lookupDefault 0 (x,y) coords) charMap | x <- [0..maxx]] |y <- [miny..maxy]])

pprints :: HM.HashMap (Int, Int) Int -> [[Int]] -> String
pprints prev (o:outs) = let (cs,ss) = pprint prev o in trace (if all (>=0) [0..10^5] then "" else " ") $ (++pprints cs outs) $! ("\n\n" ++ ss)
pprints prev [] = ""

cheat :: HM.HashMap Int Int -> HM.HashMap Int Int
cheat code = foldr (`HM.insert` 3) code [1585..1627]