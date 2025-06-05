module Day2018_05 where
import Data.Char (toUpper)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/05.txt"
    -- part 1
    print $ length $ head $ dropWhile (not . irreducible) $ iterate reduce contents
    -- part 2
    print $ minimum $ map (\x -> length $ head $ dropWhile (not . irreducible) $ iterate reduce $ filter (\c -> toUpper c /= x) contents) ['A'..'Z']

reduce :: String -> String
reduce (x:y:ys)
    | x /= y && toUpper x == toUpper y = reduce ys
    | otherwise = x : reduce (y:ys)
reduce [x] = [x]
reduce [] = []

irreducible :: String -> Bool
irreducible s = s == reduce s