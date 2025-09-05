module Day2019_23 where
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Intcode ( CompilerState(NeedsInput), parse, run, runC, getOutput )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/23.txt"
    -- part 1
    print $ last $ head $ HM.elems $ updateComps $ HM.fromList $ map (\x -> (x,runC [x] $ parse contents)) [0..49]
    -- part 2
    print $ last $ takeFirstEqual $ updateComps' 0 $ (,[]) $ HM.fromList $ map (\x -> (x,runC [x] $ parse contents)) [0..49]

updateComps :: HM.HashMap Int CompilerState -> HM.HashMap Int [Int]
updateComps comps =
    let packets = map (\[a,b,c] -> (a,[b,c])) $ sortOn (\[a,b,c] -> a) $ takeThrees $ concatMap getOutput $ HM.elems comps
        organized = foldr (\(k,v) acc -> HM.insertWith (++) k v acc) HM.empty packets
        newcomps = HM.mapWithKey (\k (NeedsInput (a,b,c,d)) -> run (HM.lookupDefault [-1] k organized) a b c) comps
        abnormal = HM.filterWithKey (\k _ -> k == 255) organized
    in if HM.null abnormal then updateComps newcomps else abnormal

updateComps' :: Int -> (HM.HashMap Int CompilerState,[Int]) -> [[Int]]
updateComps' inactiveFor (comps,nat) =
    let packets = map (\[a,b,c] -> (a,[b,c])) $ sortOn (\[a,b,c] -> a) $ takeThrees $ concatMap getOutput $ HM.elems comps
        organized = foldr (\(k,v) acc -> HM.insertWith (++) k v acc) HM.empty packets
        inactive = all null organized
        abnormal = HM.filterWithKey (\k _ -> k == 255) organized
        newNat = if HM.null abnormal then nat else lastTwo $ head $ HM.elems abnormal
        natted = if newInactive > 10 then HM.insert 0 newNat organized else organized
        newcomps = HM.mapWithKey (\k (NeedsInput (a,b,c,d)) -> run (HM.lookupDefault [-1] k natted) a b c) comps
        newInactive = if inactive then inactiveFor+1 else 0
    in (if newInactive > 10 then (newNat:) else id) $ updateComps' newInactive (newcomps,newNat)

takeThrees :: [a] -> [[a]]
takeThrees xs = if null xs then [] else take 3 xs : takeThrees (drop 3 xs)

lastTwo :: [a] -> [a]
lastTwo [x,y] = [x,y]
lastTwo (x:xs) = lastTwo xs

takeFirstEqual :: Eq a => [a] -> a
takeFirstEqual (x:y:xs)
    | x == y = x
    | otherwise = takeFirstEqual (y:xs)