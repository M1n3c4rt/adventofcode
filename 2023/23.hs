import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import Data.Foldable (maximumBy, find)
import Data.Function (on)
import Data.List (sortOn)
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "23.txt"
    let grid = adjustGrid $ lines contents
    let m = length grid - 2
    let coords = [(x,y) | x <- [1..m], y <- [1..m]]
    let nodes = filter (isNode grid) coords
    let nodemap = HM.fromList $ map (\n -> (n,getNeighbours grid nodes n)) nodes
    let nodemap' = HM.fromList $ map (\n -> (n,getNeighbours' grid nodes n)) nodes
    -- part 1
    mapM_ print $ HM.toList nodemap'
    print $ (2+) $ pathLength nodemap $ reverse $ findLongestPath nodemap (m,m) [(1,1)]
    -- part 2
    print $ (2+) $ pathLength nodemap' $ reverse $ findLongestPath nodemap' (m,m) [(1,1)]

type Node = (Int,Int)

pathLength :: HM.HashMap Node [(Node,Int)] -> [Node] -> Int
pathLength m [] = 0
pathLength m [n] = 0
pathLength m (n1:n2:ns) = pathLength m (n2:ns) + (snd . head . filter ((==n2) . fst)) (HM.lookupDefault [] n1 m)

adjustGrid :: [[Char]] -> [[Char]]
adjustGrid grid =
    let h = head grid
        newH = [head h] ++ "#" ++ drop 2 h
        l = last grid
        newL = take (length l - 2) l ++ "#" ++ [last l] in
            newH:init (tail grid) ++ [newL]

findLongestPath :: HM.HashMap Node [(Node,Int)] -> Node -> [Node] -> [Node]
findLongestPath m end path@(p:ps)
    | p == end = path
    | otherwise =
        let connected = HM.lookupDefault [] p m
            connectedFiltered = filter (\(x,n) -> x `notElem` path) connected
            candidates = map (\(x,n) -> findLongestPath m end (x:path)) connectedFiltered
            nextLongest = if null candidates then [] else maximumBy (compare `on` pathLength m . reverse) candidates
        in nextLongest

getNeighbours :: [[Char]] -> [Node] -> Node -> [(Node,Int)]
getNeighbours cs ns (x,y) = mapMaybe (expand [(x,y)]) openNeighbourDirs
    where neighbours = [(x,y-1),(x+1,y),(x-1,y),(x,y+1)]
          getC (a,b) = cs !! b !! a
          openNeighbours = filter ((/='#') . getC) neighbours
          openNeighbourDirs = map (\(a,b) -> (a,b,a-x,b-y)) openNeighbours
          expand prev (a,b,p,q)
                | (p,q) == (0,-1) && c == 'v' = Nothing
                | (p,q) == (1,0) && c == '<' = Nothing
                | (p,q) == (-1,0) && c == '>' = Nothing
                | (p,q) == (0,1) && c == '^' = Nothing
                | c == '#' = Nothing
                | (a,b) `elem` ns = Just ((a,b),1)
                | otherwise =
                    let next = expand ((a,b):prev) <$> find (\(l,m,n,o) -> (l,m) `notElem` prev && getC (l,m) /= '#') [(a,b+1,0,1),(a+1,b,1,0),(a,b-1,0,-1),(a-1,b,-1,0)] in
                    (\(Just (m,n)) -> (m,n+1)) <$> next
                where c = getC (a,b)

getNeighbours' :: [[Char]] -> [Node] -> Node -> [(Node,Int)]
getNeighbours' cs ns (x,y) = mapMaybe (expand [(x,y)]) openNeighbours
    where neighbours = [(x,y-1),(x+1,y),(x-1,y),(x,y+1)]
          getC (a,b) = cs !! b !! a
          openNeighbours = filter ((/='#') . getC) neighbours
          expand prev node@(a,b)
                | c == '#' = Nothing
                | node `elem` ns = Just (node,1)
                | otherwise =
                    let next = expand (node:prev) <$> find (\n -> n `notElem` prev && getC n /= '#') [(a,b+1),(a+1,b),(a,b-1),(a-1,b)] in
                    (\(Just (m,n)) -> (m,n+1)) <$> next
                where c = getC (a,b)

isNode :: [[Char]] -> Node -> Bool
isNode m (x,y)
    | getC (x,y) == '#' = False
    | length (filter (=='#') nc) == 2 = False
    | otherwise = True
    where neighbours = [(x,y-1),(x+1,y),(x-1,y),(x,y+1)]
          getC (a,b) = m !! b !! a
          nc = map getC neighbours