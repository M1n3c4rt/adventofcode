import Data.List (transpose, intersect, intersectBy, intercalate)
import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust, isJust)
import Data.Function (on)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "19.txt"
    let cs = zip [(0::Int)..] $ parse contents
    let tree = snd $ generateTree [] (map (\((i,p),(j,q)) -> (i,j)) $ filter (\((i,p),(j,q)) -> check p q) $ choose2 cs) 0
    -- part 1
    print $ S.size $ snd $ resolveTree (HM.fromList cs) tree
    -- part 2
    print $ maximum $ map (uncurry manhattan) $ choose2 $ S.toList $ snd $ getScanners (HM.fromList cs) tree

type Beacon = (Int,Int,Int)

parse :: String -> [[Beacon]]
parse = map (map (\l -> read ("("++l++")")) . tail . lines) . splitOn "\n\n"

rotations :: Beacon -> [Beacon]
rotations (x,y,z) =
    [
        (x,y,z), (x,-y,-z), (x,z,-y), (x,-z,y), (-x,y,-z),(-x,-y,z),
        (-x,z,y),(-x,-z,-y),(y,x,-z), (y,-x,z), (y,z,x),  (y,-z,-x),
        (-y,x,z),(-y,-x,-z),(-y,z,-x),(-y,-z,x),(z,x,y),  (z,-x,-y),
        (z,y,-x),(z,-y,x),  (-z,x,-y),(-z,-x,y),(-z,y,x), (-z,-y,-x)
    ]

rotationsInverse :: Beacon -> [Beacon]
rotationsInverse (x,y,z) =
    [
        (x,y,z), (x,-y,-z), (x,-z,y), (x,z,-y), (-x,y,-z),(-x,-y,z),
        (-x,z,y),(-x,-z,-y),(y,x,-z), (-y,x,z), (z,x,y),  (-z,x,-y),
        (y,-x,z),(-y,-x,-z),(-z,-x,y),(z,-x,-y),(y,z,x),  (-y,-z,x),
        (-z,y,x),(z,-y,x),  (y,-z,-x),(-y,z,-x),(z,y,-x), (-z,-y,-x)
    ]

offsetL :: [Beacon] -> [Beacon] -> [[Beacon]]
offsetL ls@((x,y,z):rest) os = concatMap (\(x,y,z) -> map (\(x',y',z') -> map (\(x'',y'',z'') -> (x''-x+x',y''-y+y',z''-z+z')) ls) os) ls

rotateL :: [Beacon] -> [[Beacon]]
rotateL = transpose . map rotations

permuteL :: [Beacon] -> [Beacon] -> ([Beacon] -> [Beacon])
permuteL ls os = helper $ head $ concatMap (filter ((>=12) . length . (`intersect` os) . (\(a,b,c) -> c)) . (\(i,r) -> map (i,r,) $ offsetL r os)) $ zip [0..] $ rotateL ls
    where helper (i,r,o) =
            let (x2,y2,z2) = head o
                (x1,y1,z1) = head r
            in (\t -> map (\(x,y,z) -> (x-x1+x2,y-y1+y2,z-z1+z2)) $ rotateL t !! i)

distances :: [Beacon] -> [((Beacon, Beacon), Int)]
distances = map (\ps@((a,b,c),(d,e,f)) -> (ps,(a-d)^2+(b-e)^2+(c-f)^2)) . choose2

choose2 :: [a] -> [(a,a)]
choose2 (x:xs) = map (x,) xs ++ choose2 xs
choose2 [] = []

check :: [Beacon] -> [Beacon] -> Bool
check ls os =
    let ld = map snd $ distances ls
        od = map snd $ distances os
    in length (ld `intersect` od) >= 66

data SensorTree = End Int | Tree Int [SensorTree] deriving Show

generateTree :: [Int] -> [(Int, Int)] -> Int -> ([Int],SensorTree)
generateTree finished connections c
    | null neighbours = ([c],End c)
    | otherwise = let (g,f) = foldr helper ([],c:finished) neighbours in (f,Tree c g)
    where
        lookupDouble n = foldr (\(a,b) acc -> if a == n then b:acc else if b == n then a:acc else acc) []
        neighbours = filter (`notElem` finished) $ lookupDouble c connections
        helper n (acc,f) =
            let (f',g) = generateTree f connections n
            in (g:acc,f'++f)

resolveTree :: HM.HashMap Int [Beacon] -> SensorTree -> (Int, S.Set Beacon)
resolveTree cs tree = case tree of
    End n -> (n,S.fromList $ fromJust $ HM.lookup n cs)
    Tree n ts -> (n,S.union (S.fromList $ fromJust $ HM.lookup n cs) $ S.unions (map (helper n . resolveTree cs) ts))
    where
        helper k (n,s) =
            let revert = permuteL (fromJust $ HM.lookup n cs) (fromJust $ HM.lookup k cs)
            in S.fromList $ revert $ S.toList s

getScanners :: HM.HashMap Int [Beacon] -> SensorTree -> (Int, S.Set Beacon)
getScanners cs tree = case tree of
    End n -> (n,S.singleton (0,0,0))
    Tree n ts -> (n,S.union (S.singleton (0,0,0)) $ S.unions (map (helper n . getScanners cs) ts))
    where
        helper k (n,s) =
            let revert = permuteL (fromJust $ HM.lookup n cs) (fromJust $ HM.lookup k cs)
            in S.fromList $ revert $ S.toList s

manhattan :: Num a => (a, a, a) -> (a, a, a) -> a
manhattan (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)