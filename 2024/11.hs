import Data.MemoUgly ( memo )
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- readFile "11.txt"
    let l = getNumbers contents
    -- part 1
    print $ sum $ map (\x -> blinkMemo (25,x)) l
    -- part 2
    print $ sum $ map (\x -> blinkMemo (75,x)) l
    -- keep going!
    putStr $ intercalate "\n" $ zipWith (curry (\x -> show (fst x) ++ ": " ++ show (snd x))) [75..] (map (\x -> sum $ map (\y -> blinkMemo (x,y)) l) [75..])
    
blink :: (Integer,Integer) -> Integer
blink (0,k) = 1
blink (n,k) = sum $ map (\x -> blinkMemo (n-1,x)) $ blinkOne k

blinkMemo :: (Integer,Integer) -> Integer
blinkMemo = memo blink

blinkOne :: Integer -> [Integer]
blinkOne  l
    | l == 0 = [1]
    | even (length (show l)) = let (n,l') = (length (show l) `div` 2, show l) in [read (take n l'),read (drop n l')] :: [Integer]
    | otherwise = [2024*l]

aplly :: (b -> b) -> Integer -> (b -> b)
aplly f 0 = id
aplly f n = aplly f (n-1) . f

getNumbers :: String -> [Integer]
getNumbers s = map read $ words s