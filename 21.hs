import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List (sortOn, nub, nubBy, minimumBy, intercalate, sortBy)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.MemoUgly (memo)
import qualified Data.Hashable as HM

main :: IO ()
main = do
    handle <- openFile "21.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    putStr $ show $ sum $ map (\x -> read (init x) * getShortestPaths numericMap (2,'A':x)) $ lines contents
    putStr "\n"
    -- part 2
    putStr $ show $ sum $ map (\x -> read (init x) * getShortestPaths numericMap (25,'A':x)) $ lines contents
    putStr "\n"
    --keep going!
    putStr $ intercalate "\n" $ map (\y -> show (y `div` 2) ++ ": " ++ show (sum $ map (\x -> read (init x) * getShortestPaths numericMap (y `div` 2,'A':x)) $ lines contents)) [2..]
    putStr "\n"
    hClose handle

type Dir = (Int,Int)
type Cell = (Int,Int)

numericMap :: HM.HashMap Char Cell
numericMap = HM.fromList [('7',(0,0)),('8',(1,0)),('9',(2,0)),('4',(0,1)),('5',(1,1)),('6',(2,1)),('1',(0,2)),('2',(1,2)),('3',(2,2)),('0',(1,3)),('A',(2,3))]

dirMap :: HM.HashMap Dir Cell
dirMap = HM.fromList [((0,1),(1,1)),((1,0),(2,1)),((-1,0),(0,1)),((0,-1),(1,0)),((0,0),(2,0))]

getShortestPathsMemo :: (Int, [Dir]) -> Integer
getShortestPathsMemo = memo $ getShortestPaths dirMap

getShortestPaths :: (Ord k, HM.Hashable k) => HM.HashMap k Cell -> (Int, [k]) -> Integer
getShortestPaths hm (0,inputs) = sum $ map (fromIntegral . length . head) $ getTaxicabsMulMemo hm inputs
getShortestPaths hm (l,inputs) = sum $ map (minimum . map ((\k -> getShortestPathsMemo (l-1,k)) . ((0,0):))) (getTaxicabsMulMemo hm inputs)

getTaxicabs :: HM.Hashable p => HM.HashMap p Cell -> p -> p -> [[Dir]]
getTaxicabs valid x y
    | p == 0 && q == 0 = [[(0,0)]]
    | p == 0 = [(diffSet . (\x -> x ++ [last x])) (map (a,) [b,b+q..d])]
    | q == 0 = [(diffSet . (\x -> x ++ [last x])) (map (,b) [a,a+p..c])]
    | otherwise = map (diffSet . (\x -> x ++ [last x])) (filter (all (`elem` HM.elems valid)) [map (,b) [a,a+p..(c-p)] ++ map (c,) [b,b+q..d],map (a,) [b,b+q..(d-q)] ++ map (,d) [a,a+p..c]])
    where (a,b) = HM.findWithDefault (0,0) x valid
          (c,d) = HM.findWithDefault (0,0) y valid
          (p,q) = (signum $ c-a, signum $ d-b)

getTaxicabsMulMemo :: (Ord k, HM.Hashable k) => HM.HashMap k Cell -> [k] -> [[[Dir]]]
getTaxicabsMulMemo valid = memo $ getTaxicabsMul valid

getTaxicabsMul :: (HM.Hashable k, Ord k) => HM.HashMap k Cell -> [k] -> [[[Dir]]]
getTaxicabsMul valid (p:q:qs) = getTaxicabs valid p q:getTaxicabsMulMemo valid (q:qs)
getTaxicabsMul valid [q] = []

diffSet :: [Cell] -> [Dir]
diffSet [] = []
diffSet [a] = []
diffSet ((x,y):(a,b):xs) = (a-x,b-y):diffSet ((a,b):xs)