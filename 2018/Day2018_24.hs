module Day2018_24 where
import Data.List.Split (splitOn)
import Data.List (sortOn, maximumBy, find)
import Data.Ord (Down(Down))
import Data.Function (on)
import Debug.Trace (traceShow, trace, traceShowId)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/24.txt"
    let groups = parse contents
    -- part 1
    print $ sum $ map uts $ step (length groups) groups
    -- part 2 (manual binary search because some fights can run forever)
    print $ sum $ map uts $ step (length groups) $ boost 49 groups

data Group = Group {uts::Int,hp::Int,dmg::Int,dtype::String,itv::Int,team::Bool,weak::[String],immune::[String]} deriving Show

parse :: String -> [Group]
parse contents = let [a:as,b:bs] = splitOn [""] $ lines contents in map ((\l -> l {team=True}) . parseLine) as ++ map ((\l -> l {team=False}) . parseLine) bs

parseLine :: String -> Group
parseLine line = helper $ filter (`notElem` words "units each with hit points to with an attack that does damage at initiative") $ words line
    where
        helper (w1:w2:ws) = let (w3:w4:w5:ws') = reverse ws; (a,b) = parseTraits $ reverse ws' in Group {uts=read w1,hp=read w2,dmg=read w5,dtype=w4,itv=read w3,team=True,weak=map (filter (/=',')) a,immune=map (filter (/=',')) b}
        parseTraits [] = ([],[])
        parseTraits ws@(w:rest)
            | any (';' `elem`) ws = traceShow ws $ let (as,b:bs) = break (';' `elem`) rest in
                (if w == "(weak" then id else flip) (,) (as++[init b]) (map (filter (/=')')) $ tail bs)
            | otherwise = traceShow ws $ case w of
                "(weak" -> (map (filter (/=')')) rest,[])
                "(immune" -> ([],map (filter (/=')')) rest)

select :: [Group] -> [Group] -> [(Int, Int)]
select groups (g:gs)
    | all ((==team g) . team) groups = select groups gs
    | dtype g `elem` immune candidate || effective == 0 = select groups gs
    | otherwise = (itv g,itv candidate) : select newGroups gs
    where
        effective = uts g * dmg g
        candidate = maximumBy (compare `on` (\g' -> (,uts g'*dmg g',itv g') $ (if dtype g `elem` weak g' then 2 else if dtype g `elem` immune g' then 0 else 1) * effective)) $ filter ((/=team g) . team) groups
        newGroups = filter ((/=itv candidate) . itv) groups
select groups [] = []

step :: Int -> [Group] -> [Group]
step n groups = if all team groups || not (any team groups) then
    traceShowId groups else
        traceShow (all odd [1,3..10^3],groups) $ step n $ fight groups n $ traceShowId attacks
    where
        attacks = select groups $ sortOn (Down . (\g -> (uts g*dmg g,itv g))) groups

fight :: [Group] -> Int -> [(Int,Int)] -> [Group]
fight groups 0 attacks = groups
fight groups i attacks = fight newGroups (i-1) attacks
    where
        attack = find ((==i) . fst) attacks
        newGroups' = case attack of
            Nothing -> groups
            Just (a,d) -> case liftA2 (,) (find ((==a) . itv) groups) (find ((==d) . itv) groups) of
                Nothing -> groups
                Just (atk,def) -> insert groups $ def {uts=(uts def-) $ (`div` hp def) $ (if dtype atk `elem` weak def then 2 else if dtype atk `elem` immune def then 0 else 1)*uts atk*dmg atk}
        newGroups = filter ((>0) . uts) newGroups'
        insert (g:gs) g' = if itv g == itv g' then g':gs else g:insert gs g'

boost :: Int -> [Group] -> [Group]
boost n = map (\g -> if team g then g {dmg=dmg g+n} else g)