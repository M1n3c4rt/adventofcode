import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "11.txt"
    let (updated,tiles) = run ((0,0),(0,1)) S.empty HM.empty False 0 0 $ parse contents
    let (updated',tiles') = run ((0,0),(0,1)) S.empty (HM.singleton (0,0) 1) False 0 0 $ parse contents
    -- part 1
    print $ S.size updated
    -- part 2
    putStr $ pprint $ S.fromList $ HM.keys $ HM.filter (==1) tiles'

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

run (p,d) updated tiles outputParity pointer relBase state = case inst of
    1 -> run (p,d) updated tiles outputParity (pointer+4) relBase $ HM.insert cW (a+b) state
    2 -> run (p,d) updated tiles outputParity (pointer+4) relBase $ HM.insert cW (a*b) state
    3 -> run (p,d) updated tiles outputParity (pointer+2) relBase $ HM.insert aW (HM.lookupDefault 0 p tiles) state
    4 -> if outputParity then
        run (move p d a) updated tiles False (pointer+2) relBase state
        else
        run (p,d) (S.insert p updated) (HM.insert p a tiles) True (pointer+2) relBase state
    5 -> run (p,d) updated tiles outputParity (if a == 0 then pointer+3 else b) relBase state
    6 -> run (p,d) updated tiles outputParity (if a /= 0 then pointer+3 else b) relBase state
    7 -> run (p,d) updated tiles outputParity (pointer+4) relBase $ HM.insert cW (if a < b then 1 else 0) state
    8 -> run (p,d) updated tiles outputParity (pointer+4) relBase $ HM.insert cW (if a == b then 1 else 0) state
    9 -> run (p,d) updated tiles outputParity (pointer+2) (relBase+a) state
    99 -> (updated,tiles)
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