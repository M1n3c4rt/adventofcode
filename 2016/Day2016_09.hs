module Day2016_09 where
import Utility.AOC (numbers, extrapolate)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/09.txt"
    -- part 1
    print $ length $ decompress $ filter (/='\n') contents
    -- part 2
    print $ decompress' $ filter (/='\n') contents

header (s:ss) = case s of
    '(' -> case numbers $ takeWhile (/=')') ss of
        [a,b] -> (Just (a,b),tail $ dropWhile (/=')') ss)
        _ -> (Nothing,s:ss)
    _ -> (Nothing,s:ss)

decompress ss@(s:rest) = case header ss of
    (Just (a,b),ss') -> concat (replicate b (take a ss')) ++ decompress (drop a ss')
    (Nothing,_) -> s : decompress rest
decompress [] = []

decompress' ss@(s:rest) = case header ss of
    (Just (a,b),ss') -> b * decompress' (take a ss') + decompress' (drop a ss')
    (Nothing,_) -> 1 + decompress' rest
decompress' [] = 0