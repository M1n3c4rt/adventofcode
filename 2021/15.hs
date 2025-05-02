import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import DijkstraSimple (findShortestDistance)

main :: IO ()
main = do
    contents <- readFile "15.txt"
    let cs = lines contents
        height = length cs
        width = length $ head cs
        graph = getGraph $ enumerate cs
        graph' = getGraph $ expandVertical 4 height $ expandHorizontal 4 width $ enumerate cs
    -- part 1
    print $ findShortestDistance graph (0,0) (height-1,width-1)
    -- part 2
    print $ findShortestDistance graph' (0,0) (height*5-1,width*5-1)

enumerate :: [[Char]] -> HM.HashMap (Int,Int) Int
enumerate l = let el = map (zip [0..]) l in HM.fromList $ map (\(a,b,c) -> ((b,a),read [c])) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

expandHorizontal :: Int -> Int -> HM.HashMap (Int,Int) Int -> HM.HashMap (Int,Int) Int
expandHorizontal 0 width ms = ms
expandHorizontal n width ms = HM.union ms $ expandHorizontal (n-1) width $ HM.map (\n -> if n /= 9 then n+1 else 1) $ HM.mapKeys (\(x,y) -> (x+width,y)) ms

expandVertical :: Int -> Int -> HM.HashMap (Int,Int) Int -> HM.HashMap (Int,Int) Int
expandVertical 0 height ms = ms
expandVertical n height ms = HM.union ms $ expandVertical (n-1) height $ HM.map (\n -> if n /= 9 then n+1 else 1) $ HM.mapKeys (\(x,y) -> (x,y+height)) ms

getGraph :: HM.HashMap (Int,Int) Int -> HM.HashMap (Int,Int) [((Int,Int),Int)]
getGraph ms = HM.foldrWithKey helper HM.empty ms
    where
        helper (x,y) v = HM.insert (x,y) (mapMaybe (\p -> (p,) <$> HM.lookup p ms) [(x,y+1),(x+1,y),(x,y-1),(x-1,y)])