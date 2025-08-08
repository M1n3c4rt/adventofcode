module Day2016_07 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/07.txt"
    -- part 1
    print $ length $ filter validate $ lines contents
    -- part 2
    print $ length $ filter validate' $ lines contents


supernet :: String -> String
supernet (s:ss) = case s of
    '[' -> s : supernet (dropWhile (/=']') ss)
    _ -> s : supernet ss
supernet [] = []

hypernet :: Bool -> String -> String
hypernet inside (s:ss) = case s of
    '[' -> s : hypernet True ss
    ']' -> s : hypernet False ss
    _ -> (if inside then (s:) else id) $ hypernet inside ss
hypernet _ [] = []

abba :: String -> Bool
abba ss = case take 4 ss of
    [a,b,c,d] -> abba (tail ss) || a /= b && b == c && a == d
    _ -> False

aba :: Eq a => [a] -> [[a]]
aba ss = case take 3 ss of
    [a,b,c] -> if a == c && a /= b then [a,b,c] : aba (tail ss) else aba (tail ss)
    _ -> []

invertaba :: [a] -> [a]
invertaba [a,b,c] = [b,a,b]

validate :: String -> Bool
validate s = abba (supernet s) && not (abba (hypernet False s))

validate' :: String -> Bool
validate' s = any (`elem` map invertaba (aba (hypernet False s))) $ aba $ supernet s