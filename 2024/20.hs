import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "20.txt"
    -- part 1
    let g = getGraph $ lines contents in print $ getCheats 2 100 g
    -- part 2
    let g = getGraph $ lines contents in print $ getCheats 20 100 g
    
type Node = (Int,Int)

enumerate :: [[a]] -> [(Int,Int,a)]
enumerate l = let el = map (zip [0..]) l in map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

findS :: [(Int,Int,Char)] -> (Int,Int)
findS el = (\(a,b,c) -> (a,b)) $ head $ filter (\(a,b,c) -> c == 'S') el

extend :: [String] -> (Node,Int) -> [(Node, Int)]
extend l ((x,y),n) = filter (\((a,b),c) -> l !! b !! a `elem` ".E") [((x,y+1),n+1),((x,y-1),n+1),((x+1,y),n+1),((x-1,y),n+1)]

getNext :: [(Node,Int)] -> [String] -> [(Node,Int)]
getNext (p:prev) l
    | null next = p:prev
    | otherwise = getNext (next ++ p:prev) l
    where next = filter (\((a,b),c) -> not $ any (\((x,y),z) -> (a,b) == (x,y)) prev) $ extend l p

getGraph :: [[Char]] -> HM.HashMap Node Int
getGraph l = HM.fromList $ getNext [(findS $ enumerate l,0)] l

getCheats :: Int -> Int -> HM.HashMap Node Int -> Int
getCheats n min graph = sum $ HM.elems $ HM.mapWithKey (\k v -> getCheat 1 n min k v graph) graph

getCheat :: Int -> Int -> Int -> Node -> Int -> HM.HashMap Node Int -> Int
getCheat step n min (x,y) dist graph
    | step > n = 0
    | otherwise = getCheat (step+1) n min (x,y) dist graph + length (filter (\m -> m-dist-step >= min) $ filter (/=(-1)) $ map (\node -> HM.findWithDefault (-1) node graph) $ genSquare (x,y) step)

genSquare :: Node -> Int -> [Node]
genSquare (x,y) n = [(x+k,y+n-k) | k <- [0..n-1]] ++ [(x+n-k,y-k) | k <- [0..n-1]] ++ [(x-k,y-n+k) | k <- [0..n-1]] ++ [(x-n+k,y+k) | k <- [0..n-1]]