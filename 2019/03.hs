import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.List (genericReplicate, minimumBy)
import Data.Function (on)

main :: IO ()
main = do
    contents <- readFile "03.txt"
    -- part 1
    print $ (\[x,y] -> getMinIntersection x y) $ map parse $ lines contents
    -- part 2
    print $ (\[x,y] -> getMinIntersection' x y) $ map parse $ lines contents

parse :: String -> [(Int, Int)]
parse line = scanl helper (0,0) $ splitOn "," line
    where
        helper (x,y) (c:rest) = case c of
            'R' -> (x+r,y)
            'L' -> (x-r,y)
            'U' -> (x,y+r)
            'D' -> (x,y-r)
            where r = read rest

intersection :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Maybe ((Int,Int),(Int, Int))
intersection p@((a,b),(c,d)) q@((e,f),(g,h))
    | a == c && f == h = if (b-f)*(d-f) <= 0 && (e-a)*(g-a) <= 0 && (a,f) /= (0,0) then Just ((abs (f-b), abs (e-a)),(a,f)) else Nothing
    | b == d && e == g = intersection q p
    | otherwise = Nothing

takePairs :: [a] -> [(a, a)]
takePairs (x:y:ys) = (x,y):takePairs (y:ys)
takePairs [x] = []
takePairs [] = []

takePairsWithDist :: Num b => b -> [(b, b)] -> [(b, ((b, b), (b, b)))]
takePairsWithDist n ((p,q):(r,s):ys) = (n,((p,q),(r,s))):takePairsWithDist (n + abs (r-p) + abs (s-q)) ((r,s):ys)
takePairsWithDist n [x] = []
takePairsWithDist n [] = []

getMinIntersection :: [(Int, Int)] -> [(Int, Int)] -> Int
getMinIntersection as bs =
    minimum $ map (\(x,y) -> abs x + abs y) $ catMaybes [snd <$> intersection p q | p <- takePairs as, q <- takePairs bs]

getMinIntersection' :: [(Int, Int)] -> [(Int, Int)] -> Int
getMinIntersection' as bs = minimum $
    catMaybes [(\((x,y),c) -> x+m+y+n) <$> intersection p q | (m,p) <- takePairsWithDist 0 as, (n,q) <- takePairsWithDist 0 bs]