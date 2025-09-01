module Day2015_05 where
import Data.List.Extra (chunksOf, group, sort)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/05.txt"
    -- part 1
    print $ length $ filter nice $ lines contents
    -- part 2
    print $ length $ filter nice' $ lines contents

nice :: String -> Bool
nice s = (length (filter (`elem`"aeiou") s) >= 3) &&
    any ((>=2) . length) (group s) &&
    not (any (`elem` ["ab","cd","pq","xy"]) $ chunksOf 2 s ++ chunksOf 2 (tail s))

nice' :: String -> Bool
nice' s = any (uncurry (==)) (choosePairs s) && helper s
    where
        helper (a:b:c:ss) = a == c || helper (b:c:ss)
        helper _ = False

choosePairs :: [a] -> [([a],[a])]
choosePairs [a,b] = []
choosePairs (a:b:ss) = map ([a,b],) (chunksOf 2 ss ++ chunksOf 2 (tail ss)) ++ choosePairs (b:ss)