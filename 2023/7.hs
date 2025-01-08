import Data.List (sort, sortOn, nub)

main :: IO ()
main = do
    contents <- readFile "7.txt"
    -- part 1
    print $ sum $ zipWith (*) [1..] $ map snd $ sortOn fst $ parse contents
    -- part 2
    print $ sum $ zipWith (*) [1..] $ map snd $ sortOn fst $ parse' contents

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show,Eq,Ord,Enum)
data Card' = Joker | Only Card deriving (Show,Eq,Ord)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show,Eq,Ord)
data Hand = Hand Card Card Card Card Card deriving (Show,Eq)
data Hand' = Hand' Card' Card' Card' Card' Card' deriving (Show,Eq)

parse :: String -> [(Hand,Int)]
parse = map ((\[a,b] -> (fromString a, read b)) . words) . lines

parse' :: String -> [(Hand',Int)]
parse' = map ((\[a,b] -> (fromString' a, read b)) . words) . lines

getHandType :: Hand -> HandType
getHandType (Hand p q r s t) = case (n,m) of
    (1,1) -> FiveOfAKind
    (2,1) -> FourOfAKind
    (2,2) -> FullHouse
    (3,1) -> ThreeOfAKind
    (3,2) -> TwoPair
    (4,1) -> OnePair
    (5,0) -> HighCard
    where l@[a,b,c,d,e] = sort [p,q,r,s,t]
          n = length (nub l)
          unNub (c:cs) | null cs = [] | otherwise = if c `elem` cs then c:unNub cs else unNub cs
          m = length $ nub (unNub l)


getAllJokerHands :: [Card'] -> [[Card]]
getAllJokerHands cs = map (\c' -> map (\c -> if c == Joker then c' else (\(Only x) -> x) c) cs) [Two .. Ace]

getHandType' :: Hand' -> HandType
getHandType' (Hand' p q r s t) = maximum $ map (getHandType . (\[a,b,c,d,e] -> Hand e d c b a)) (getAllJokerHands [p,q,r,s,t])

fromString :: String -> Hand
fromString = (\[a,b,c,d,e] -> Hand a b c d e) . map (\c -> snd $ head $ filter ((==c) . fst) $ zip "23456789TJQKA" [Two .. Ace])

fromString' :: String -> Hand'
fromString' = (\[a,b,c,d,e] -> Hand' a b c d e) . map fromChar'
    where fromChar' c
            | c == 'J' = Joker
            | otherwise = snd $ head $ filter ((==c) . fst) $ zip "23456789TJQKA" $ map Only [Two .. Ace]

instance Ord Hand where
    (<=) :: Hand -> Hand -> Bool
    (<=) h1@(Hand a b c d e) h2@(Hand p q r s t) = let (ht1, ht2) = (getHandType h1, getHandType h2) in
        if ht1 /= ht2 then ht1 <= ht2 else
            [a,b,c,d,e] <= [p,q,r,s,t]

instance Ord Hand' where
    (<=) :: Hand' -> Hand' -> Bool
    (<=) h1@(Hand' a b c d e) h2@(Hand' p q r s t) = let (ht1, ht2) = (getHandType' h1, getHandType' h2) in
        if ht1 /= ht2 then ht1 <= ht2 else
            [a,b,c,d,e] <= [p,q,r,s,t]