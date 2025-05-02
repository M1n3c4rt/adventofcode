import qualified Data.HashMap.Strict as HM
import Data.List (findIndex, elemIndex, nub)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "10.txt"
    let grid = lines contents
    let network = convertToNodes $ enumerate grid
    let pipe = findPipe network (Ring [] (findS grid) [])
    -- part 1
    print $ lengthr pipe
    -- part 2
    print $ S.size $ S.filter (\(x,y) -> even x && even y) $ floodFill (S.singleton (140,140)) S.empty $ S.fromList $ expandPipe $ mergeRing pipe

data Ring a = Ring [a] a [a] deriving (Show)
type Network = HM.HashMap (Int,Int) ((Int,Int),(Int,Int))

inBounds :: (Ord a, Num a) => (a, a) -> Bool
inBounds (a,b) = min a b >= 0 && max a b < (139*2 - 1)

lengthr :: Ring a -> Int
lengthr (Ring r a l) = length r

mergeRing :: Ring a -> [a]
mergeRing (Ring r a l) = r ++ [a] ++ reverse l

expandPipe :: [(Int, Int)] -> [(Int, Int)]
expandPipe [(x,y)] = []
expandPipe ((a,b):(c,d):ds) = (2*a,2*b):(a+c,b+d):expandPipe ((c,d):ds)

findS :: [[Char]] -> (Int,Int)
findS sss = let Just p = findIndex ('S' `elem`) sss in
                let Just q = elemIndex 'S' (sss !! p) in (p,q)

floodFill :: S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
floodFill frontier finished blocks
    | S.null frontier = finished
    | otherwise = floodFill newfrontier (S.union frontier finished) blocks
    where newfrontier = let unfilteredNeighbours = S.unions $ S.map (\(x,y) -> S.fromList [(x+1,y),(x,y-1),(x,y+1),(x-1,y)]) frontier in
            S.filter (\p -> inBounds p && all (S.notMember p) [finished,frontier,blocks]) unfilteredNeighbours

enumerate :: [[a]] -> [(Int,Int,a)]
enumerate l = let el = map (zip [0..]) l in map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

findPipe :: Network -> Ring (Int, Int) -> Ring (Int, Int)
findPipe n cur = case cur of
    Ring [] a [] -> let (c1,c2) = HM.lookupDefault ((0,0),(0,0)) a n in findPipe n $ Ring [c1] a [c2]
    Ring l@(c1:c1s) a r@(c2:c2s) -> if c1 == c2 then cur else
        let (c1a,c1b) = HM.lookupDefault ((0,0),(0,0)) c1 n in
        let (c2a,c2b) = HM.lookupDefault ((0,0),(0,0)) c2 n in
            findPipe n $ Ring ((if c1a `elem` c1s then c1b else c1a):l) a ((if c2a `elem` c2s then c2b else c2a):r)

convertToNodes :: [(Int,Int,Char)] -> Network
convertToNodes = HM.fromList . map f
    where f (x,y,'|') = ((x,y),((x,y+1),(x,y-1)))
          f (x,y,'-') = ((x,y),((x+1,y),(x-1,y)))
          f (x,y,'L') = ((x,y),((x+1,y),(x,y-1)))
          f (x,y,'F') = ((x,y),((x+1,y),(x,y+1)))
          f (x,y,'7') = ((x,y),((x-1,y),(x,y+1)))
          f (x,y,'J') = ((x,y),((x-1,y),(x,y-1)))
          f (x,y,'.') = ((x,y),((x,y),(x,y)))
          f (x,y,'S') = ((x,y),((x-1,y),(x,y-1)))