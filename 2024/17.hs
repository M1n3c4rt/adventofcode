
import Data.List.Split ( splitOn )
import Data.Bits ( Bits(xor) )
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- readFile "17.txt"
    -- part 1
    putStrLn $ intercalate "," $ map show $ reverse $ process $ getInfo contents
    -- part 2
    print $ minimum $ genNumber $ getInsts contents

type Pointer = Int
type Out = [Int]
type Mem = (Pointer,Int,Int,Int,Out,[Int])

getInfo :: String -> Mem
getInfo s = let [a,b] = splitOn "\n\n" s
            in let [ra,rb,rc] = map (read . tail . dropWhile (/= ':')) $ lines a
            in (0,ra,rb,rc,[],map read $ splitOn "," $ tail $ dropWhile (/= ':') b)

getInsts :: String -> [Int]
getInsts s = let [_,b] = splitOn "\n\n" s
            in map read $ splitOn "," $ tail $ dropWhile (/= ':') b

process :: Mem -> Out
process (p,ra,rb,rc,out,insts)
    | p >= (length insts - 1) = out
    | otherwise = let (mem,op) = ((p,ra,rb,rc,out,insts),insts !! (p+1)) in case insts !! p of
        0 -> process (p+2,ra `div` (2^combo mem op),rb,rc,out,insts)
        1 -> process (p+2,ra,rb `xor` op,rc,out,insts)
        2 -> process (p+2,ra,combo mem op `mod` 8,rc,out,insts)
        3 -> if ra == 0 then process (p+2,ra,rb,rc,out,insts) else process (op,ra,rb,rc,out,insts)
        4 -> process (p+2,ra,rb `xor` rc,rc,out,insts)
        5 -> process (p+2,ra,rb,rc,(combo mem op `mod` 8):out,insts)
        6 -> process (p+2,ra,ra `div` (2^combo mem op),rc,out,insts)
        7 -> process (p+2,ra,rb,ra `div` (2^combo mem op),out,insts)

process' :: Mem -> [Mem]
process' (p,ra,rb,rc,out,insts)
    | p >= (length insts - 1) = [(p,ra,rb,rc,out,insts)]
    | otherwise = let (mem,op) = ((p,ra,rb,rc,out,insts),insts !! (p+1)) in case insts !! p of
        0 -> mem:process' (p+2,ra `div` (2^combo mem op),rb,rc,out,insts)
        1 -> mem:process' (p+2,ra,rb `xor` op,rc,out,insts)
        2 -> mem:process' (p+2,ra,combo mem op `mod` 8,rc,out,insts)
        3 -> if ra == 0 then mem:process' (p+2,ra,rb,rc,out,insts) else mem:process' (op,ra,rb,rc,out,insts)
        4 -> mem:process' (p+2,ra,rb `xor` rc,rc,out,insts)
        5 -> mem:process' (p+2,ra,rb,rc,(combo mem op `mod` 8):out,insts)
        6 -> mem:process' (p+2,ra,ra `div` (2^combo mem op),rc,out,insts)
        7 -> mem:process' (p+2,ra,rb,ra `div` (2^combo mem op),out,insts)

getDigit :: Int -> Int
getDigit a = ((a `mod` 8) `xor` (a `div` (2^(7-(a `mod` 8))))) `mod` 8

genNumber :: [Int] -> [Int]
genNumber [] = []
genNumber [n] = map fst $ filter (\x -> snd x == n) $ zip [0..] $ map getDigit [0..7]
genNumber (n:ns) = let a = map (*8) $ genNumber ns in map fst $ concatMap (filter (\x -> snd x == n) . (\x -> zip (map (+x) [0..7]) (map (getDigit . (+x)) [0..7]))) a

genList :: Int -> Out
genList n = process (0,n,0,0,[],[2,4,1,7,7,5,1,7,0,3,4,1,5,5,3,0])

combo :: Mem -> Int -> Int
combo (p,ra,rb,rc,out,insts) op = case op of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> ra
    5 -> rb
    6 -> rc