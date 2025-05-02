import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "18.txt"
    let snumbers = map (readSnumber 0) $ lines contents
    -- part 1
    print $ magnitude $ foldl1 add snumbers
    -- part 2
    print $ maximum $ map (\(a,b) -> max (magnitude $ add a b) (magnitude $ add b a)) $ choose2 snumbers

type Snumber = [(Int,Int)]

readSnumber :: Int -> String -> Snumber
readSnumber recl (s:ss) = case s of
    '[' -> readSnumber (recl+1) ss
    ']' -> readSnumber (recl-1) ss
    ',' -> readSnumber recl ss
    d -> (read [d],recl) : readSnumber recl ss
readSnumber _ "" = []

explode :: Snumber -> Snumber
explode [] = []
explode [a] = [a]
explode [a,b]
    | snd a == 5 && snd b == 5 = [(0,4)]
    | otherwise = [a,b]
explode [a,b,c]
    | snd a == 5 && snd b == 5 = [(0,4),(fst c + fst b, snd c)]
    | snd b == 5 && snd c == 5 = [(fst a + fst b, snd a),(0,4)]
    | otherwise = [a,b,c]
explode (a:b:c:d:ds)
    | snd a == 5 && snd b == 5 = (0,4):(fst c + fst b, snd c):d:ds
    | snd b == 5 && snd c == 5 = (fst a + fst b, snd a):(0,4):(fst d + fst c, snd d):ds
    | otherwise = a:explode (b:c:d:ds)

split :: Snumber -> Snumber
split (a:bs)
    | fst a >= 10 = (fst a `div` 2,snd a + 1):(fst a-(fst a `div` 2),snd a + 1):bs
    | otherwise = a:split bs
split [] = []

reduce :: Snumber -> Snumber
reduce n =
    let exploded = explode n
        splitted = split n
    in
        if exploded /= n then
            reduce exploded
        else if splitted /= n then
            reduce splitted
        else n

add :: Snumber -> Snumber -> Snumber
add sn1 sn2 = reduce (map (\(a,b) -> (a,b+1)) sn1 ++ map (\(a,b) -> (a,b+1)) sn2)

magnitude :: Snumber -> Int
magnitude ns =
    let scores = map ((2^) . (4-) . snd) ns
        score = sum scores
        halfPoint = length $ takeWhile (<=(score `div` 2)) $ scanl1 (+) scores
        (left,right) = splitAt halfPoint ns
    in if length ns == 1 then fst $ head ns else 3*magnitude left + 2*magnitude right

choose2 :: [a] -> [(a,a)]
choose2 (x:xs) = map (x,) xs ++ choose2 xs
choose2 [] = []