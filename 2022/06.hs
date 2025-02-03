import Data.List (nub)

main :: IO ()
main = do
    contents <- readFile "06.txt"
    -- part 1
    print $ firstMarker 4 contents
    -- part 2
    print $ firstMarker 14 contents

firstMarker :: Int -> String -> Int
firstMarker n s@(c:cs)
    | nub packet == packet = n
    | otherwise = 1 + firstMarker n cs
    where packet = take n s