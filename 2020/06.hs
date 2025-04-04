import Data.List.Split (splitOn)
import Data.List (nub, intersect)

main :: IO ()
main = do
    contents <- readFile "06.txt"
    let resolve f = print $ sum $ map (length . nub . f . lines) $ splitOn "\n\n" contents
    -- part 1
    resolve concat
    -- part 2
    resolve $ foldr intersect ['a'..'z']