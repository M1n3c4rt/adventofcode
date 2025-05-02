import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.List (nubBy)

main :: IO ()
main = do
    contents <- readFile "20.txt"
    let ms = processCons $ parseModules $ lines contents
    -- part 1
    print $ (\(a,b,c) -> b*c*((1000`div`a)^2)) $ processPulses 1000 ms [("button",False)] []
    -- part 2
    print $ foldl lcm 1 $ map (\i -> processPulses' (i,"zr",True) ms [("button",False)] []) ["xf","cm","sz","gc"]

data Module = Button | Broadcaster [String] | Flip Bool [String] | Con (HM.HashMap String Bool) [String] deriving Show
type Modules = HM.HashMap String Module

parseModules :: [String] -> HM.HashMap String Module
parseModules [] = HM.singleton "button" Button
parseModules (m:ms)
    | a == '%' = HM.insert as (Flip False outs) $ parseModules ms
    | a == '&' = HM.insert as (Con HM.empty outs) $ parseModules ms
    | otherwise = HM.insert (a:as) (Broadcaster outs) $ parseModules ms
    where [a:as,b] = splitOn " -> " m
          outs = splitOn ", " b

processCons :: Modules -> Modules
processCons modules = HM.mapWithKey helper modules
    where helper k v = case v of
            Con m ss -> Con (HM.fromList $ map (,False) $ getInputs k modules) ss
            _ -> v

getInputs :: String -> Modules -> [String]
getInputs name = HM.keys . HM.filter helper
    where helper v = case v of
            Button -> name == "broadcaster" 
            Broadcaster ss -> name `elem` ss
            Flip _ ss -> name `elem` ss
            Con _ ss -> name `elem` ss

processPulses :: Int -> Modules -> [(String,Bool)] -> [(String,String,Bool)] -> (Int,Int,Int)
processPulses n ms ((i,pulse):ins) outs = case HM.lookupDefault (Broadcaster []) i ms of
    Button -> processPulses n ms ins ((i,"broadcaster",False):outs)
    Broadcaster ss -> processPulses n ms ins (map (i,,pulse) ss ++ outs)
    Flip b ss -> if pulse then processPulses n ms ins outs else let newms = HM.adjust (\(Flip c ss) -> Flip (not c) ss) i ms in
        processPulses n newms ins (map (i,,not b) ss ++ outs)
    Con cins ss ->  processPulses n ms ins (map (i,,not $ and (HM.elems cins)) ss ++ outs)

processPulses n ms [] []
    | n == 1 = (1,0,0)
    | not $ or $ HM.elems $ HM.map helper ms = (1,0,0)
    | otherwise = let (c,l,h) = processPulses (n-1) ms [("button",False)] [] in (c+1,l,h)
    where helper v = case v of
            Flip b _ -> b
            Con m _ -> or $ HM.elems m
            _ -> False

processPulses n ms [] outs =
    let pulses = map (\(a,b,c) -> c) outs
        combine (low,high) (cycle,l,h) = (cycle,l+low,h+high) in
    combine (length $ filter not pulses, length $ filter id pulses) $ processPulses n newms (map (\(a,b,c) -> (b,c)) outs) []
    where newms = HM.mapWithKey helper ms
          helper k v = case v of
            Con m ss -> Con (HM.union (HM.fromList $ map (\(a,b,c) -> (a,c)) $ filter (\(a,b,c) -> b == k) outs) m) ss
            _ -> v

processPulses' :: (String,String,Bool) -> Modules -> [(String,Bool)] -> [(String,String,Bool)] -> Int
processPulses' t ms ((i,pulse):ins) outs = case HM.lookupDefault (Broadcaster []) i ms of
    Button -> processPulses' t ms ins ((i,"broadcaster",False):outs)
    Broadcaster ss -> processPulses' t ms ins (map (i,,pulse) ss ++ outs)
    Flip b ss -> if pulse then processPulses' t ms ins outs else let newms = HM.adjust (\(Flip c ss) -> Flip (not c) ss) i ms in
        processPulses' t newms ins (map (i,,not b) ss ++ outs)
    Con cins ss ->  processPulses' t ms ins (map (i,,not $ and (HM.elems cins)) ss ++ outs)

processPulses' t ms [] [] = 1 + processPulses' t ms [("button",False)] []

processPulses' t@(i,o,pulse) ms [] outs
    | null i && (o,pulse) `elem` ins = 1
    | t `elem` outs = 1
    | otherwise = processPulses' t newms ins []
    where ins = map (\(a,b,c) -> (b,c)) outs
          newms = HM.mapWithKey helper ms
          helper k v = case v of
            Con m ss -> Con (HM.union (HM.fromList $ map (\(a,b,c) -> (a,c)) $ filter (\(a,b,c) -> b == k) outs) m) ss
            _ -> v