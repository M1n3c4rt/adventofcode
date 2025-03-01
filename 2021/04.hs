import Data.List.Split (splitOn)
import Data.List (transpose)
import Debug.Trace (trace)

main :: IO ()
main = do
    contents <- readFile "04.txt"
    let (ns, cards) = parse contents
    -- part 1
    print $ punchCards ns cards
    -- part 2
    print $ punchCards' ns cards []

type Card = [[(Int,Bool)]]

parse :: [Char] -> ([Int], [Card])
parse contents =
    let n:cs = splitOn "\n\n" contents
        ns = splitOn "," n
        parseCard = map (map ((,True) . read) . words) . lines
    in (map read ns,map parseCard cs)

punchCards :: [Int] -> [Card] -> Int
punchCards (n:ns) cs =
    let updateCard = map (map (\(k,b) -> if k==n then (k,not b) else (k,b)))
        newcs = map updateCard cs
        winners = filter isWinner newcs
        isWinner cs = not (all (any snd) cs) || not (all (any snd) $ transpose cs)
    in if null winners then punchCards ns newcs else getScore n $ head winners

punchCards' :: [Int] -> [Card] -> Card -> Int
punchCards' (n:ns) cs pc =
    let updateCard = map (map (\(k,b) -> if k==n then (k,not b) else (k,b)))
        newcs = map updateCard cs
        notWinners = filter isNotWinner newcs
        isNotWinner cs = not (not (all (any snd) cs) || not (all (any snd) $ transpose cs))
    in if null notWinners then getScore n (updateCard pc) else punchCards' ns newcs (head notWinners)

getScore :: Int -> Card -> Int
getScore n c = (n*) $ sum $ map fst $ concatMap (filter snd) c