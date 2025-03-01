
main :: IO ()
main = do
    contents <- readFile "07.txt"
    let ns = parse contents
    -- part 1
    print $ windowBy (\n -> abs . (n-)) ns
    -- part 2
    print $ windowBy (\n k -> let d = abs (n-k) in (d*(d+1)) `div` 2) ns

parse :: (Num a, Read a) => String -> [a]
parse cs = read $ "[" ++ cs ++ "]"

windowBy :: (Int -> Int -> Int) -> [Int] -> Int
windowBy f ns = minimum $ map helper [a..b]
    where
        helper n = sum $ map (f n) ns
        a = minimum ns
        b = maximum ns