module Day2016_21 where
import Data.List (sortOn, find, permutations)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/21.txt"
    -- part 1
    putStrLn $ map snd $ sortOn fst $ foldl op (zip [0..] "abcdefgh") $ map parse $ lines contents
    -- part 2
    putStrLn $ head $ filter ((=="fbgdceah") . (\y -> map snd $ sortOn fst $ foldl op (zip [0..] y) $ map parse $ lines contents)) $ permutations "abcdefgh"

data StringInst = SwapP Int Int | SwapL Char Char | Rotate Int | RotateB Char | Reverse Int Int | Move Int Int deriving Show

parse :: String -> StringInst
parse l = case words l of
    ["swap","position",x,_,_,y] -> SwapP (read x) (read y)
    ["swap","letter",x,_,_,y] -> SwapL (head x) (head y)
    ["rotate","left",x,_] -> Rotate (-read x)
    ["rotate","right",x,_] -> Rotate (read x)
    ["rotate","based",_,_,_,_,x] -> RotateB (head x)
    ["reverse",_,x,_,y] -> Reverse (read x) (read y)
    ["move",_,x,_,_,y] -> Move (read x) (read y)

op :: [(Int, Char)] -> StringInst -> [(Int, Char)]
op s (SwapP a b) = map (\(n,c) -> if n == a then (b,c) else if n == b then (a,c) else (n,c)) s
op s (SwapL x y) = map (\(n,c) -> if c == x then (n,y) else if c == y then (n,x) else (n,c)) s
op s (Rotate a) = map (\(n,c) -> ((n+a) `mod` 8,c)) s
op s (RotateB x) = op s (Rotate (1+i+(if i >= 4 then 1 else 0)))
    where Just (i,_) = find ((==x) . snd) s
op s (Reverse a b) = map (\(n,c) -> if n >= a && n <= b then (a+b-n,c) else (n,c)) s
op s (Move a b) =
    if b > a then
        map (\(n,c) -> if n == a then (b,c) else if n > a && n <= b then (n-1,c) else (n,c)) s
    else
        map (\(n,c) -> if n == a then (b,c) else if n >= b && n < a then (n+1,c) else (n,c)) s

unop :: [(Int, Char)] -> StringInst -> [(Int, Char)]
unop s (SwapP a b) = op s (SwapP a b)
unop s (SwapL x y) = op s (SwapL x y)
unop s (Rotate a) = op s (Rotate (-a))
unop s (RotateB x) = op s (Rotate 0)
    where Just (i,_) = find ((==x) . snd) s
unop s (Reverse a b) = op s (Reverse a b)
unop s (Move a b) = op s (Move b a)