import qualified Data.HashMap.Strict as HM
import DijkstraSimple ( findShortestDistance )
import Data.Maybe ( mapMaybe )
import Data.Char (ord)

main :: IO ()
main = do
    contents <- readFile "12.txt"
    let m = enumerate $ lines contents
    let [s,e] = findEnds m
    let as = findAs m
    let g = getGraph $ clean m
    -- part 1
    print $ findShortestDistance g s e
    -- part 2
    print $ minimum $ map (\start -> findShortestDistance g start e) as

enumerate :: [[Char]] -> HM.HashMap (Int,Int) Char
enumerate l = let el = map (zip [0..]) l in HM.fromList $ map (\(a,b,c) -> ((b,a),c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

clean :: HM.HashMap k Char -> HM.HashMap k Char
clean = HM.map (\c -> if c == 'S' then 'a' else if c == 'E' then 'z' else c)

enumerate' :: [[Char]] -> [(Int,Int,Int)]
enumerate' l = let el = map (zip [0..]) l in map (\(a,b,c) -> (b,a,ord c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

findEnds :: HM.HashMap (Int,Int) Char -> [(Int,Int)]
findEnds m = "SE" >>= (\c -> HM.keys $ HM.filter (==c) m)

findAs :: HM.HashMap (Int,Int) Char -> [(Int,Int)]
findAs m = HM.keys $ HM.filter (`elem` "Sa") m

getNeighbours :: HM.HashMap (Int,Int) Char -> (Int,Int) -> Char -> [((Int,Int),Int)]
getNeighbours m (x,y) a =
    let coords = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        neighbours = filter ((\c -> ord c <= ord a + 1) . (\c -> HM.lookupDefault 'z' c m)) coords
    in map (,1) neighbours

getGraph :: HM.HashMap (Int, Int) Char -> HM.HashMap (Int, Int) [((Int, Int), Int)]
getGraph m = HM.mapWithKey (getNeighbours m) m