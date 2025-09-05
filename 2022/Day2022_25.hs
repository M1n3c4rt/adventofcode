module Day2022_25 where


main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/25.txt"
    -- part 1
    putStrLn $ showSNAFU $ sum $ map readSNAFU $ lines contents
    -- part 2

readSNAFU :: String -> Int
readSNAFU s = r $ reverse s
    where
        r "" = 0
        r (c:ss) = n c + 5 * r ss
            where
                n s' = case s' of
                    '=' -> -2
                    '-' -> -1
                    '0' ->  0
                    '1' ->  1
                    '2' ->  2

showSNAFU :: Int -> String
showSNAFU 0 = ""
showSNAFU n =
    let (q,r) = n `divMod` 5
        rToChar k = case k of
            0 -> '0'
            1 -> '1'
            2 -> '2'
            3 -> '='
            4 -> '-'
    in showSNAFU (if r > 2 then q+1 else q) ++ [rToChar r]