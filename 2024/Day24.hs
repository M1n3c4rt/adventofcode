module Day24 where

import Data.List ( sort, sortOn )
import qualified Data.HashMap.Strict as HM
import Data.List.Split ( splitOn )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2024/24.txt"
    let (bits,gates) = getBoth contents
    -- part 1
    print $ getFinal $ simplify bits [] gates
    -- part 2 (did this manually)
    putStrLn "fgt,fpq,nqk,pcp,srn,z07,z24,z32"

data Gate = XOR | AND | OR | NONE deriving (Read,Show)
data Tree a = End a | Node a (Tree a) (Tree a) deriving (Read,Show)

flattenMaybe :: [Maybe a] -> [a]
flattenMaybe (l:ls) = case l of
    Nothing -> flattenMaybe ls
    Just a -> a:flattenMaybe ls
flattenMaybe [] = []

getAllZs :: [(String, String, Gate, String)] -> [String]
getAllZs gates = sort $ filter (\x -> head x == 'z') $ map (\(a,b,c,d) -> d) gates

getYs :: Int -> HM.HashMap String Bool -> Int
getYs n = bintodec . map snd . take n . sortOn fst . HM.toList . HM.filterWithKey (\k _ -> head k == 'y')

getXs :: Int -> HM.HashMap String Bool -> Int
getXs n = bintodec . map snd . take n . sortOn fst . HM.toList . HM.filterWithKey (\k _ -> head k == 'x')

evaluateUntil :: Int -> HM.HashMap String Bool -> [(String, String, Gate, String)] -> Int
evaluateUntil n bits gates = bintodec $ map (evaluateTree bits . getTree gates) $ take n ["z00","z01","z02","z03","z04","z05","z06","z07","z08","z09","z10","z11","z12","z13","z14","z15","z16","z17","z18","z19","z20","z21","z22","z23","z24","z25","z26","z27","z28","z29","z30","z31","z32","z33","z34","z35","z36","z37","z38","z39","z40","z41","z42","z43","z44","z45"]

parse :: HM.HashMap String Bool -> String -> String -> Gate -> Bool
parse map a b gate = let (a',b') = (HM.findWithDefault False a map, HM.findWithDefault False b map) in case gate of
    AND -> a' && b'
    OR -> a' || b'
    XOR -> a' /= b'

getBits :: String -> HM.HashMap String Bool
getBits = HM.fromList . map (\x -> let [a,b] = splitOn ": " x in (a,b == "1")) . lines

getGates :: String -> [(String, String, Gate, String)]
getGates = map (\x -> let [a,b,c,d,e] = words x in (a,c,read b,e)) . lines

getBoth :: String -> (HM.HashMap String Bool, [(String, String, Gate, String)])
getBoth s = let [a,b] = splitOn "\n\n" s in (getBits a, getGates b)

simplify :: HM.HashMap String Bool -> [(String,String,Gate,String)] -> [(String,String,Gate,String)] -> HM.HashMap String Bool
simplify bits next (g@(a,b,gate,out):gates)
    | HM.member a bits && HM.member b bits = simplify ((HM.insert out $ parse bits a b gate) bits) next gates
    | otherwise = simplify bits (g:next) gates
simplify bits [] [] = bits
simplify bits next [] = simplify bits [] next

getTree :: [(String,String,Gate,String)] -> String -> Tree (String,Gate)
getTree ns s
    | null ns' = End (s,NONE)
    | otherwise = let (a,b,gate,out) = head ns' in Node (s,gate) (getTree ns a) (getTree ns b)
    where ns' = filter (\(a,b,c,d) -> d == s) ns

getFinal :: HM.HashMap [Char] Bool -> Int
getFinal = bintodec . map snd . sortOn fst . HM.toList . HM.filterWithKey (\k _ -> head k == 'z')

bintodec :: [Bool] -> Int
bintodec = foldr (\x y -> fromEnum x + 2*y) 0

size :: Tree a2 -> Int
size tree = case tree of
    End a -> 1
    Node a ta tb -> 1 + size ta + size tb

skew :: Tree a2 -> Tree a2
skew tree = case tree of
    End a -> End a
    Node a ta tb -> if size ta >= size tb then Node a (skew ta) (skew tb) else Node a (skew tb) (skew ta)

prettyPrint :: (Show a) => Int -> Tree a -> String
prettyPrint indent tree = case tree of
    End a -> take indent (cycle "| ") ++ show a
    Node a (End b) (End c) -> take indent (cycle "| ") ++ show a ++ " " ++ show b ++ " " ++ show c
    Node a ta tb -> take indent (cycle "| ") ++ show a ++ "\n" ++ prettyPrint (indent+2) ta ++ "\n" ++ prettyPrint (indent+2) tb

getTreeNum :: Tree (String,Gate) -> Int
getTreeNum (Node a (End (p, NONE)) (End (q, NONE))) = read $ tail p
getTreeNum tree = 0

evaluateTree :: HM.HashMap String Bool -> Tree (String,Gate) -> Bool
evaluateTree bits tree = case tree of
    Node (a, XOR) ta tb -> evaluateTree bits ta /= evaluateTree bits tb
    Node (a, OR) ta tb -> evaluateTree bits ta || evaluateTree bits tb
    Node (a, AND) ta tb -> evaluateTree bits ta && evaluateTree bits tb
    End (a, NONE) -> HM.lookupDefault False a bits