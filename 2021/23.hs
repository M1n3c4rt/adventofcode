import Data.List (elemIndex, sort, delete, minimumBy)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.HashMap.Strict as HM
import Data.MemoUgly (memo)
import Data.Function (on)
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "23.txt"
    -- part 1
    print $ snd $ fromJust $ minEnergy (0,parse contents)
    -- part 2
    print $ snd $ fromJust $ minEnergy' (0,parse' contents)

type State = ([(Int,Int)],HM.HashMap Int [Int])

energyFromColumn :: Int -> Int
energyFromColumn n = 10^((n-2) `div` 2)

reachable :: [Int] -> Int -> [(Int,Int)] -> [Int] -> Bool
reachable frontier b hw finished
    | b `elem` h = False
    | b `elem` finished = True
    | null frontier = False
    | otherwise = reachable newfrontier b hw (frontier++finished)
    where
        newfrontier = concatMap (filter (`notElem` h) . filter (\n -> n >= 0 && n <= 10) . filter (`notElem` finished) . (\n -> [n-1,n+1])) frontier
        h = map fst hw

parse :: String -> State
parse ss = ([],) $ HM.fromList $ zip [2,4,6,8] $ map (map num) $ (\[a,b,c,d,e,f,g,h] -> [[a,e],[b,f],[c,g],[d,h]]) $ map (flip (!!)) [31,33,35,37,45,47,49,51] <*> pure ss
    where
        num 'A' = 2
        num 'B' = 4
        num 'C' = 6
        num 'D' = 8

nexts :: State -> [(Int,State)]
nexts (hw,cs) =
    let hwcandidates = filter (\(n,t) -> reachable [n] t hw [] && all (==t) (fromJust $ HM.lookup t cs)) hw
        hwstates = map (\(n,t) -> ((energyFromColumn t *) $ abs (n-t) + 2 - length (fromJust $ HM.lookup t cs),(delete (n,t) hw,HM.adjust (t:) t cs))) hwcandidates
        csstates = HM.foldrWithKey helper [] cs
        helper k v a
          | all (==k) v = a
          | otherwise =
            let reachables = filter (\v -> reachable [k] v hw []) [0,1,3,5,7,9,10]
                lengths = map (\n -> (n, (energyFromColumn (head v) *) $ abs (k-n) + 3 - length v)) reachables
                states = map (\(n,l) -> (l,((n,head v):hw,HM.adjust tail k cs))) lengths
            in states ++ a
    in if null hwstates then csstates else hwstates

minEnergy :: (Int, State) -> Maybe ([(Int,State)],Int)
minEnergy (n,state)
    | state == ([],HM.fromList [(2,[2,2]),(4,[4,4]),(6,[6,6]),(8,[8,8])]) = Just ([(n,state)],n)
    | otherwise = Data.Bifunctor.bimap ((n,state):) (n+) <$> helperMemo state

helper :: State -> Maybe ([(Int,State)],Int)
helper state = let ns = mapMaybe minEnergy $ nexts state in if null ns then Nothing else Just $ minimumBy (compare `on` snd) ns
helperMemo :: State -> Maybe ([(Int,State)], Int)
helperMemo = memo helper

parse' :: String -> State
parse' ss = ([],) $ HM.fromList $ zip [2,4,6,8] $ (\[a,b,c,d,e,f,g,h] -> [[num a,8,8,num e],[num b,6,4,num f],[num c,4,2,num g],[num d,2,6,num h]]) $ map (flip (!!)) [31,33,35,37,45,47,49,51] <*> pure ss
    where
        num 'A' = 2
        num 'B' = 4
        num 'C' = 6
        num 'D' = 8

nexts' :: State -> [(Int,State)]
nexts' (hw,cs) =
    let hwcandidates = filter (\(n,t) -> reachable [n] t hw [] && all (==t) (fromJust $ HM.lookup t cs)) hw
        hwstates = map (\(n,t) -> ((energyFromColumn t *) $ abs (n-t) + 4 - length (fromJust $ HM.lookup t cs),(delete (n,t) hw,HM.adjust (t:) t cs))) hwcandidates
        csstates = HM.foldrWithKey helper [] cs
        helper k v a
          | all (==k) v = a
          | otherwise =
            let reachables = filter (\v -> reachable [k] v hw []) [0,1,3,5,7,9,10]
                lengths = map (\n -> (n, (energyFromColumn (head v) *) $ abs (k-n) + 5 - length v)) reachables
                states = map (\(n,l) -> (l,((n,head v):hw,HM.adjust tail k cs))) lengths
            in states ++ a
    in if null hwstates then csstates else hwstates

minEnergy' :: (Int, State) -> Maybe ([(Int,State)],Int)
minEnergy' (n,state)
    | state == ([],HM.fromList [(2,[2,2,2,2]),(4,[4,4,4,4]),(6,[6,6,6,6]),(8,[8,8,8,8])]) = Just ([(n,state)],n)
    | otherwise = Data.Bifunctor.bimap ((n,state):) (n+) <$> helperMemo' state

helper' :: State -> Maybe ([(Int,State)],Int)
helper' state = let ns = mapMaybe minEnergy' $ nexts' state in if null ns then Nothing else Just $ minimumBy (compare `on` snd) ns
helperMemo' :: State -> Maybe ([(Int,State)], Int)
helperMemo' = memo helper'