
main :: IO ()
main = do
    contents <- readFile "02.txt"
    -- part 1
    print $ length $ filter validate $ map parse $ lines contents
    -- part 2
    print $ length $ filter validate' $ map parse $ lines contents

parse :: [Char] -> (Int, Int, Char, [Char])
parse line =
    let start = takeWhile (/='-') line
        end = takeWhile (/=' ') $ tail $ dropWhile (/='-') line
        c = last $ takeWhile (/=':') line
        pass = drop 2 $ dropWhile (/=':') line
    in (read start,read end,c,pass)

validate :: (Int, Int, Char, [Char]) -> Bool
validate (a,b,c,pass) = let count = length $ filter (==c) pass in count >= a && count <= b

validate' :: (Int, Int, Char, [Char]) -> Bool
validate' (a,b,c,pass) = (==1) $ length $ filter id [pass !! (a-1) == c , pass !! (b-1) == c]