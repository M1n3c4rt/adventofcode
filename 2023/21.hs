import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "21.txt"
    let b = blocks $ lines contents
    -- part 1
    print $ S.size $ step 64 b $ S.singleton (65,65)
    -- part 2
    print $ sum $ fullstep 202300 b

inBounds :: (Ord a, Num a) => (a, a) -> Bool
inBounds (a,b) = min a b >= 0 && max a b < 131

enumerate :: [[a]] -> [(Int,Int,a)]
enumerate l = let el = map (zip [0..]) l in map (\(a,b,c) -> (b,a,c)) $ concat $ zipWith (\x y -> map (\t -> (x, fst t, snd t)) y) [0..] el

blocks :: [[Char]] -> S.Set (Int, Int)
blocks = S.fromList . map (\(a,b,c) -> (a,b)) . filter (\(a,b,c) -> c == '#') . enumerate

step' :: (Int,Int) -> Int -> S.Set (Int,Int) -> Int
step' (p,q) n b =
    let square =  S.fromList [(x,y) | x <- [p-n..p+n], y <- [q-n..q+n]] in
        S.size $ S.filter (\a@(x,y) -> p `mod` 2 + q `mod` 2 == x `mod` 2 + y `mod` 2 && abs (p-x) + abs (q-y) <= n && inBounds a && S.notMember a b) square

step :: Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
step 0 b frontier = frontier
step n b frontier = step (n-1) b newFrontier
    where unfilteredNewFrontier = S.unions $ S.map (\(x,y) -> S.fromList [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]) frontier
          newFrontier = S.filter (\p -> S.notMember p b && inBounds p) unfilteredNewFrontier

fullstep :: Int -> S.Set (Int,Int) -> [Int]
fullstep n bl = zipWith (*) [1,1,1,1,n^2,(n-1)^2,n,n,n,n,n-1,n-1,n-1,n-1] $ map s l
    where l = [(130,65,0),(130,65,130),(130,0,65),(130,130,65),(130,65,65),(131,65,65),(64,0,0),(64,130,0),(64,0,130),(64,130,130),(195,0,0),(195,130,0),(195,0,130),(195,130,130)]
          s (a,b,c) = S.size $ step a bl $ S.singleton (b,c)