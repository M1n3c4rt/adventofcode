import Utility.AOC
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "01.txt"
    -- part 1
    print $ fromJust $ findProduct 2020 $ map read $ lines contents
    -- part 2
    print $ findProduct2 2020 $ map read $ lines contents

findProduct :: (Eq t, Num t) => t -> [t] -> Maybe t
findProduct n (l:ls) =
    if (n-l) `elem` ls then Just (l*(n-l)) else findProduct n ls
findProduct n [] = Nothing

findProduct2 :: (Eq t, Num t, Show t) => t -> [t] -> t
findProduct2 n (l:ls) = case findProduct (n-l) ls of
    Just k -> l*k
    Nothing -> findProduct2 n ls