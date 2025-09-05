{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Day2016_11 where
import qualified Data.HashMap.Strict as HM
import Utility.AOC (choose)
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import Data.List (sort, nubBy)
import qualified Data.Set as S
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/11.txt"
    -- both parts of this code take several hours to run. i'm not optimizing it any further.
    -- part 1
    let initState = (0,) $ HM.fromList $ zip [0..] $ map parse $ lines contents
    print $ floodFill S.empty (S.singleton initState) $ goal initState
    -- part 2
    let initState' = (0,) $ HM.adjust (S.union $ S.fromList [Gen "elerium",Chip "elerium",Gen "dilithium",Chip "dilithium"]) 0 $ HM.fromList $ zip [0..] $ map parse $ lines contents
    print $ floodFill S.empty (S.singleton initState') $ goal initState'

data Item = Chip String | Gen String deriving (Show, Eq, Ord, Generic)
type State = (Int,HM.HashMap Int (S.Set Item))

parse :: String -> S.Set Item
parse s = S.fromList $ map helper matches
    where
        matches = filter (`notElem` words "The first second third fourth floor contains nothing relevant a generator and microchip") $ words $ filter (`notElem` ",.") s
        helper w = if '-' `elem` w then Chip (takeWhile (/='-') w) else Gen w

neighbours :: State -> S.Set State
neighbours (n,m) =
    let is = map S.fromList $ choose 2 is' ++ choose 1 is';
        is' = S.toList $ HM.lookupDefault S.empty n m in S.fromList $ filter isValid $
    map (\i -> (n+1,) $ HM.adjust (S.union i) (n+1) $ HM.insert n (S.fromList $ filter (`S.notMember` i) is') m) is
    ++ if all (S.null . fromJust . (`HM.lookup` m)) [0..n-1] then [] else map (\i -> (n-1,) $ HM.adjust (S.union i) (n-1) $ HM.insert n (S.fromList $ filter (`S.notMember` i) is') m) is

isValid :: State -> Bool
isValid (n,m) = n `elem` [0..3] && not (any (`elem`[-1,4]) $ HM.keys m) && not (any fried $ HM.elems m)
    where
        isChip i = case i of Chip _ -> True; _ -> False
        fried is = not $ all (\case Chip s -> Gen s `elem` is || all isChip is; Gen _ -> True) is

goal :: State -> State
goal (_,m) = (3,HM.fromList [(0,S.empty),(1,S.empty),(2,S.empty),(3,S.unions $ HM.elems m)])

floodFill :: S.Set State -> S.Set State -> State -> Int
floodFill finished frontier goal'
    | goal' `S.member` frontier = 0
    | otherwise = 1 + floodFill (clean $ S.union frontier finished) (clean $ S.unions $ S.map (S.filter (`notElem'` finished) . neighbours) frontier) goal'
    where clean = S.fromList . nubBy equiv . S.toList

equiv :: State -> State -> Bool
equiv (n,m) (n',m') = n == n' && pairs m == pairs m'
    where
        element (Gen s) = s; element (Chip s) = s
        items = S.toList . S.map element . S.unions . HM.elems
        pairs k = sort $ map (\i -> map (\x -> head $ HM.keys $ HM.filter (x `S.member`) k) [Gen i,Chip i]) $ items k
        
instance Hashable Item where
    hash :: Item -> Int
    hash (Gen a) = hash a
    hash (Chip a) = hash (reverse a)

notElem' :: State -> S.Set State -> Bool
notElem' x xs = not $ any (equiv x) xs