module Day23 where
import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.List (sortOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/23.txt"
    -- part 1
    print $ last $ head $ HM.elems $ updateComps $ HM.fromList $ map (\x -> (x,run [x] 0 0 $ parse contents)) [0..49]
    -- part 2
    print $ last $ takeFirstEqual $ updateComps' 0 $ (,[]) $ HM.fromList $ map (\x -> (x,run [x] 0 0 $ parse contents)) [0..49]

data CompilerState = NeedsInput (Int,Int,HM.HashMap Int Int,[Int]) | Finished [Int] deriving Show

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

cons :: Int -> CompilerState -> CompilerState
cons x (Finished xs) = Finished (x:xs)
cons x (NeedsInput (a,b,c,xs)) = NeedsInput (a,b,c,x:xs)

run :: [Int] -> Int -> Int -> HM.HashMap Int Int -> CompilerState
run inputs pointer relBase state = case inst of
    1 -> run inputs (pointer+4) relBase $ HM.insert cW (a+b) state
    2 -> run inputs (pointer+4) relBase $ HM.insert cW (a*b) state
    3 -> if null inputs then NeedsInput (pointer,relBase,state,[]) else
        run (tail inputs) (pointer+2) relBase $ HM.insert aW (head inputs) state
    4 -> cons a $ run inputs (pointer+2) relBase state
    5 -> run inputs (if a == 0 then pointer+3 else b) relBase state
    6 -> run inputs (if a /= 0 then pointer+3 else b) relBase state
    7 -> run inputs (pointer+4) relBase $ HM.insert cW (if a < b then 1 else 0) state
    8 -> run inputs (pointer+4) relBase $ HM.insert cW (if a == b then 1 else 0) state
    9 -> run inputs (pointer+2) (relBase+a) state
    99 -> Finished []
    where
        (inst:mods) = splitMode $ HM.lookupDefault 0 pointer state

        vals@(a':b':c':vs) = map (\x -> HM.lookupDefault 0 (pointer+x) state) [1..]
        refs@(a :b :c :rs) = zipWith applyMod vals mods
        refWrites@(aW:bW:cW:rws) = zipWith applyModW vals mods

        applyMod x' m = case m of
            0 -> HM.lookupDefault 0 x' state
            1 -> x'
            2 -> HM.lookupDefault 0 (relBase + x') state

        applyModW x' m = case m of
            0 -> x'
            2 -> relBase + x'

splitMode :: (Integral a, Read a, Show a) => a -> [a]
splitMode n = n `mod` 100 : map (read . pure) (reverse $ show $ n `div` 100) ++ repeat 0

updateComps :: HM.HashMap Int CompilerState -> HM.HashMap Int [Int]
updateComps comps =
    let packets = map (\[a,b,c] -> (a,[b,c])) $ sortOn (\[a,b,c] -> a) $ takeThrees $ concatMap (\(NeedsInput (a,b,c,d)) -> d) $ HM.elems comps
        organized = foldr (\(k,v) acc -> HM.insertWith (++) k v acc) HM.empty packets
        newcomps = HM.mapWithKey (\k (NeedsInput (a,b,c,d)) -> run (HM.lookupDefault [-1] k organized) a b c) comps
        abnormal = HM.filterWithKey (\k _ -> k == 255) organized
    in if HM.null abnormal then updateComps newcomps else abnormal

updateComps' :: Int -> (HM.HashMap Int CompilerState,[Int]) -> [[Int]]
updateComps' inactiveFor (comps,nat) =
    let packets = map (\[a,b,c] -> (a,[b,c])) $ sortOn (\[a,b,c] -> a) $ takeThrees $ concatMap (\(NeedsInput (a,b,c,d)) -> d) $ HM.elems comps
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

takeFirstEqual (x:y:xs)
    | x == y = x
    | otherwise = takeFirstEqual (y:xs)