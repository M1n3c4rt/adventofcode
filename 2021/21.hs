import Data.Complex
import Data.MemoUgly (memo)

main :: IO ()
main = do
    contents <- readFile "21.txt"
    let [a,b] = parse contents
    -- part 1
    print $ deterministic False (cycle [1..100]) a b 0 0 0
    -- part 2
    print $ mc $ diracMemo (False,a,b,0,0)

parse :: String -> [Int]
parse = map (read . drop 28) . lines

deterministic :: Bool -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int
deterministic parity dice a b at bt numrolls
    | at >= 1000 = bt*numrolls
    | bt >= 1000 = at*numrolls
    | otherwise =
        if parity then
            let newb = ((b+sum (take 3 dice)-1) `mod` 10) + 1 in deterministic (not parity) (drop 3 dice) a newb at (bt+newb) (numrolls+3)
        else
            let newa = ((a+sum (take 3 dice)-1) `mod` 10) + 1 in deterministic (not parity) (drop 3 dice) newa b (at+newa) bt (numrolls+3)

dirac :: (Bool, Int, Int, Int, Int) -> Complex Int
dirac (parity,a,b,at,bt)
    | at >= 21 = 1 :+ 0
    | bt >= 21 = 0 :+ 1
    | otherwise =
        if parity then
            let newbs = map (\(n,w) -> (((b+n-1) `mod` 10) + 1,w)) rolls3 in foldl1 (/+) $ map (\(n,w) -> w/*diracMemo (not parity,a,n,at,bt+n)) newbs
        else
            let newas = map (\(n,w) -> (((a+n-1) `mod` 10) + 1,w)) rolls3 in foldl1 (/+) $ map (\(n,w) -> w/*diracMemo (not parity,n,b,at+n,bt)) newas

diracMemo :: (Bool, Int, Int, Int, Int) -> Complex Int
diracMemo = memo dirac

rolls3 :: [(Int, Int)]
rolls3 = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

(/+) :: Num a => Complex a -> Complex a -> Complex a
(a:+b)/+(c:+d) = (a+c):+(b+d)

(/*) :: Num a => a -> Complex a -> Complex a
k/*(a:+b) = (k*a):+(k*b)

mc :: Ord a => Complex a -> a
mc (a:+b) = max a b