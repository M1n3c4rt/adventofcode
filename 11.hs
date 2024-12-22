import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.MemoUgly ( memo )
import Data.List (intercalate)

main :: IO ()
main = do
    handle <- openFile "11.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    putStr $ show $ sum $ map (\x -> blinkMemo (25,x)) $ getNumbers contents
    putStr "\n"
    -- part 2
    putStr $ show $ sum $ map (\x -> blinkMemo (75,x)) $ getNumbers contents
    putStr "\n"
    -- keep going!
    let l = getNumbers contents in putStr $ intercalate "\n" $ zipWith (curry (\x -> show (fst x) ++ ": " ++ show (snd x))) [75..] (map (\x -> sum $ map (\y -> blinkMemo (x,y)) l) [75..])
    putStr "\n"
    hClose handle

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