import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "13.txt"
    let grids = map lines $ splitOn "\n\n" contents
    -- part 1
    print $ sum $ map meq grids
    -- part 2
    print $ sum $ map meq' grids

meq :: [[Char]] -> Int
meq ss = let eqs = map (\s -> filter (uncurry (==) . getHalves s) [1..length s-1]) ss in
    let rows = filter (uncurry (==) . getHalves ss) [1..length ss-1] in
        let cols = filter (\e -> all (e `elem`) eqs) (head eqs) in
            if null rows then head cols else 100 * head rows

meq' :: [[Char]] -> Int
meq' ss = let eqs = map (\s -> filter (uncurry (==) . getHalves s) [1..length s-1]) ss;
                eqs' = map (\s -> filter (uncurry (=~) . getHalves s) [1..length s-1]) ss in
    let rows = filter (uncurry (=~~) . getHalves ss) [1..length ss-1] in
        let cols = concatMap (filter (\e -> length (filter (e `notElem`) eqs) == 1 && length (filter (e `elem`) eqs') == 1)) eqs in
            if null rows then head cols else 100 * head rows

(=~) :: Eq a => [a] -> [a] -> Bool
a =~ b = length (filter id $ zipWith (/=) a b) == 1

(=~~) :: Eq a => [[a]] -> [[a]] -> Bool
a =~~ b = length (filter id $ zipWith (=~) a b) == 1 && length (filter id $ zipWith (/=) a b) == 1

getHalves :: [a] -> Int -> ([a], [a])
getHalves ss n
    | length a > length b = (drop (length a - length b) a,reverse b)
    | length b > length a = (reverse a,take (length a) b)
    | otherwise = (a,b)
    where (a,b) = splitAt n ss