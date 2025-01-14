import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "02.txt"
    -- part 1
    print $ sum $ getValidIDs $ map parse $ lines contents
    -- part 2
    print $ sum $ map (power . parse) $ lines contents

data Cubes a = Red a | Blue a | Green a deriving Show
data Color = R | B | G deriving (Show,Eq)

comp :: Cubes Int -> Bool
comp (Red a) = a <= 12
comp (Blue a) = a <= 14
comp (Green a) = a <= 13

color :: Cubes a -> Color
color c = case c of
    Red a -> R
    Blue a -> B
    Green a -> G

getNum :: Cubes a -> a
getNum c = case c of
    Red a -> a
    Blue a -> a
    Green a -> a

power :: [[Cubes Int]] -> Int
power cs = product $ map ((maximum . map getNum) . (\c -> concatMap (filter (\x -> color x == c)) cs)) [R,G,B]

isValid :: [[Cubes Int]] -> Bool
isValid = all (all comp)

getValidIDs :: [[[Cubes Int]]] -> [Int]
getValidIDs ls = map fst . filter (isValid . snd) $ zip [1..] ls

parse :: [Char] -> [[Cubes Int]]
parse line = map (map (subparse . tail) . splitOn ",") (splitOn ";" $ tail $ dropWhile (/= ':') line)

subparse :: [Char] -> Cubes Int
subparse s
    | b == "red" = Red n
    | b == "blue" = Blue n
    | b == "green" = Green n
    where [a,b] = splitOn " " s; n = read a