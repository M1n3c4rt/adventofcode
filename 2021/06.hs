import Data.MemoUgly ( memo )
import Data.List (intercalate, genericLength)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "06.txt"
    let l = getNumbers contents
    -- part 1
    print $ sum $ map (\x -> blinkMemo (80,x)) l
    -- part 2
    print $ sum $ map (\x -> blinkMemo (256,x)) l
    -- keep going!
    mapM_ putStrLn $ zipWith (curry (\x -> show (fst x) ++ ": " ++ show (snd x))) [257..] (map (\x -> sum $ map (\y -> blinkMemo (x,y)) l) [257..])
    
blink :: (Int,Int) -> Integer
blink (0,k) = 1
blink (n,k) = sum $ map (\x -> blinkMemo (n-1,x)) $ blinkOne k

blinkMemo :: (Int,Int) -> Integer
blinkMemo = memo blink

blinkOne :: Int -> [Int]
blinkOne  l
    | l == 0 = [6,8]
    | otherwise = [l-1]

getNumbers :: String -> [Int]
getNumbers s = map read $ splitOn "," s