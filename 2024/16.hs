import Data.List ( nub )
import DijkstraSimple ( findShortestDistance,findShortestPaths,fromDistance,graphFromList )

main :: IO ()
main = do
    contents <- readFile "16.txt"
    let g = graphFromList $ getGraph $ lines contents
    -- part 1
    print $ fromDistance (findShortestDistance g (1,length (lines contents) - 2,1,0) (length (lines contents) - 2,1,0,-1))
    -- part 2
    print $ length $ nub $ concatMap (padL . fst) $ findShortestPaths g (1,length (lines contents) - 2,1,0) (length (lines contents) - 2,1,0,-1)

type Node = (Int,Int,Int,Int)
type Path = (Int,[Node])

pad :: Node -> Node -> [(Int,Int)]
pad (a,b,c,d) (p,q,r,s)
    | (a,b) == (p,q) = [(a,b)]
    | otherwise = (a,b):pad (a-c,b-d,c,d) (p,q,r,s)

padL :: [Node] -> [(Int,Int)]
padL (m:n:ns) = pad m n ++ padL (n:ns)
padL [(a,b,c,d)] = [(a,b)]
padL [] = []

enumerate :: [[a]] -> [(Int,Int,a)]
enumerate l = let el = map (zip [0..]) l in map (\x -> (snd3 x, fst3 x, thd3 x)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

getNode :: [String] -> (Int,Int) -> [Node]
getNode l (x,y)
    | l !! y !! x == '#' = []
    | l !! y !! x == 'S' = (x,y,1,0):filter (\(p,q,a,b) -> l !! (y-b) !! (x-a) == '.') [(x,y,0,1),(x,y,0,-1),(x,y,-1,0)]
    | l !! (y-1) !! x == '#' && l !! (y+1) !! x == '#' && l !! y !! (x-1) /= '#' && l !! y !! (x+1) /= '#' = []
    | l !! (y-1) !! x /= '#' && l !! (y+1) !! x /= '#' && l !! y !! (x-1) == '#' && l !! y !! (x+1) == '#' = []
    | otherwise = filter (\(p,q,a,b) -> l !! (y-b) !! (x-a) == '.') [(x,y,0,1),(x,y,1,0),(x,y,0,-1),(x,y,-1,0)]

isNode :: [String] -> (Int,Int) -> Bool
isNode l (x,y)
    | l !! y !! x == '#' = False
    | l !! y !! x == 'S' = True
    | l !! (y-1) !! x == '#' && l !! (y+1) !! x == '#' && l !! y !! (x-1) /= '#' && l !! y !! (x+1) /= '#' = False
    | l !! (y-1) !! x /= '#' && l !! (y+1) !! x /= '#' && l !! y !! (x-1) == '#' && l !! y !! (x+1) == '#' = False
    | otherwise = True

expand :: [String] -> Node -> [(Node,Int)]
expand l (x,y,a,b) = filter (\x -> snd x `notElem` [0,1000]) [extend l (x,y,a,b), addT 1000 $ extend l (x,y,-b,a), addT 1000 $ extend l (x,y,b,-a)]

extend :: [String] -> Node -> (Node, Int)
extend l (x,y,a,b)
    | l !! (y+b) !! (x+a) == '#' = ((x,y,a,b),0)
    | isNode l (x+a,y+b) = ((x+a,y+b,a,b),1)
    | otherwise = addT 1 $ extend l (x+a,y+b,a,b)

getGraph :: [String] -> [(Node, [(Node, Int)])]
getGraph l = map (\x -> (x,expand l x)) $ concatMap (getNode l . (\x -> (fst3 x, snd3 x))) (enumerate l)

addT :: Num b => b -> (a, b) -> (a, b)
addT n x = (fst x, snd x + n)

fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b
thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c