module Day2015_10 where
import Data.List (group)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/10.txt"
    -- part 1
    print $ length $ (!!40) $ iterate step contents
    -- part 2
    print $ length $ (!!50) $ iterate step contents

step :: String -> String
step = concatMap (\l -> show (length l) ++ [head l]) . group