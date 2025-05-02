{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
import Data.List.Split (splitOn)
import Data.List (transpose, intercalate)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, mapMaybe, fromJust, fromMaybe)
import Utility.AOC (neighbours4, enumerateFilter)
import Debug.Trace (trace, traceShowId, traceShow)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "20.txt"
    let solved = fromJust $ addPieces HM.empty [(0,0)] $ parse contents
    let pixels = S.size $ S.fromList $ enumerateFilter (=='#') $ unlines $ combine solved
    -- part 1
    putStrLn $ pprint solved
    mapM_ putStrLn $ combine solved
    print $ findCornerMul solved
    print $ (pixels-) $ length $ search $ combine solved
    -- part 2

serpent :: [(Int, Int)]
serpent = [(0,0),(1,1),(4,1),(5,0),(6,0),(7,1),(10,1),(11,0),(12,0),(13,1),(16,1),(17,0),(18,0),(18,-1),(19,0)]

parse :: String -> [(Int,[String])]
parse = concatMap helper . splitOn "\n\n"
    where helper chunk =
            let ([a],b) = splitAt 1 $ lines chunk
            in map (read $ take 4 $ drop 5 a,) $ rotate8 b

rotate8 :: [[a]] -> [ [[a]] ]
rotate8 ls =
    [
        id,
        map reverse,
        reverse . transpose,
        transpose,
        reverse,
        map reverse . reverse,
        map reverse . reverse . transpose,
        transpose . reverse
    ] <*> pure ls

addPieces :: HM.HashMap (Int,Int) (Int,[String]) -> [(Int,Int)] -> [(Int,[String])] -> Maybe (HM.HashMap (Int,Int) (Int,[String]))
addPieces finished open [] = Just finished
addPieces finished open pieces = if null checked then Nothing else Just $ head checked
    where
        checked = concatMap (mapMaybe (\f -> addPieces f (newOpen $ HM.keys f) (newPieces f pieces)) . helper finished open) pieces
        helper f o p = mapMaybe (check f p) o
        check f' (n,p') (x,y)
            | and $ catMaybes
                [
                    (==head p') . last . snd <$> HM.lookup (x,y+1) f',
                    (==last p') . head . snd <$> HM.lookup (x,y-1) f',
                    (==map head p') . map last . snd <$> HM.lookup (x-1,y) f',
                    (==map last p') . map head . snd <$> HM.lookup (x+1,y) f'
                ] = Just (HM.insert (x,y) (n,p') f')
            | otherwise = Nothing
        newPieces f = filter (\(n,p) -> n `notElem` HM.elems (HM.map fst f))
        newOpen cs = concatMap (filter (`notElem` cs) . neighbours4) cs

pprint :: HM.HashMap (Int,Int) (Int,[String]) -> String
pprint finished = if HM.null finished then "" else
    let (minx,miny,maxx,maxy) = bounds finished
    in unlines $ map ((unwords . map (\c -> maybe "    " (show <$> fst) (HM.lookup c finished))) . (\n -> map (n,) [miny..maxy])) [minx..maxx]

combine :: HM.HashMap (Int,Int) (Int,[String]) -> [String]
combine finished = if HM.null finished then [""] else
    let (minx,miny,maxx,maxy) = bounds finished
        strip = map (tail . init) . tail . init
    in concatMap ((map concat . transpose . map (\c -> strip $ transpose $ reverse $ snd $ fromJust (HM.lookup c finished))) . (\n -> map (n,) [miny..maxy])) [minx..maxx]

findCornerMul :: HM.HashMap (Int,Int) (Int,[String]) -> Int
findCornerMul finished =
    let (minx,miny,maxx,maxy) = bounds finished
    in product $ map (\c -> fst $ fromJust $ HM.lookup c finished) [(minx,miny),(maxx,miny),(minx,maxy),(maxx,maxy)]

search :: [[Char]] -> [(Int, Int)]
search grid =
    let grids = rotate8 grid
        search1 ss = let g = S.fromList $ enumerateFilter (=='#') $ unlines ss in S.filter (all (`S.member` g)) $ S.map (\(x,y) -> map (\(x',y') -> (x'+x,y'+y)) serpent) g
    in concat $ S.unions $ map search1 grids

bounds :: HM.HashMap (Int, Int) v -> (Int, Int, Int, Int)
bounds finished =
    let minx = minimum $ map fst $ HM.keys finished
        miny = minimum $ map snd $ HM.keys finished
        maxx = maximum $ map fst $ HM.keys finished
        maxy = maximum $ map snd $ HM.keys finished
    in (minx,miny,maxx,maxy)