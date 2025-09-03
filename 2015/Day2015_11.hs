module Day2015_11 where
import Data.List (group, nub)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/11.txt"
    -- part 1
    print $ head $ filter valid $ iterate (reverse . increment . reverse) contents
    -- part 2
    print $ (!!1) $ filter valid $ iterate (reverse . increment . reverse) contents

increment :: String -> String
increment ('z':p) = 'a':increment p
increment (c:p) = succ c:p

dec :: String -> Bool
dec (a:b:c:pass) = (c == succ b && b == succ a) || dec (b:c:pass)
dec _ = False

valid :: String -> Bool
valid pass = dec pass && ((>=2) . length . nub . map (take 2) . filter ((>=2) . length) . group) pass && not (any (`elem`"iol") pass)