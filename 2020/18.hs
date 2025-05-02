import Debug.Trace (traceShowId)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "18.txt"
    -- part 1
    print $ sum $ map (solveExp . readExp . reverse . splitTokens) $ lines contents
    -- part 2
    print $ sum $ map (solveExp . readExp' . splitTokens) $ lines contents

data Exp = Num Int | Sum Exp Exp | Product Exp Exp deriving Show

readExp :: [String] -> Exp
readExp (a:b:tokens)
    | b == "*" = Product (readExp [a]) (readExp tokens)
    | b == "+" = Sum (readExp [a]) (readExp tokens)
readExp [a] = if length a == 1 then Num (read a) else readExp (reverse $ splitTokens a)

readExp' :: [String] -> Exp
readExp' s
    | length s == 1 && length (head s) == 1 = Num (read $ head s)
    | length s == 1 = readExp' (splitTokens $ head s)
    | "*" `elem` s = foldl1 Product $ map readExp' $ splitOn ["*"] s
    | otherwise = foldl1 Sum $ map readExp' $ splitOn ["+"] s

splitTokens :: String -> [String]
splitTokens cs@(c:rest)
    | c == '(' = let (a,b) = takeNextBracket 1 "" rest in a:splitTokens b
    | otherwise = [c]:if null rest then [] else splitTokens (tail rest)
    where
        takeNextBracket indent consumed (c:cs)
            | c == '(' = takeNextBracket (indent+1) (c:consumed) cs
            | c == ')' = if indent == 1 then (reverse consumed,drop 1 cs) else takeNextBracket (indent-1) (c:consumed) cs
            | otherwise = takeNextBracket indent (c:consumed) cs
splitTokens [] = []

solveExp :: Exp -> Int
solveExp exp = case exp of
    Num n -> n
    Sum a b -> solveExp a + solveExp b
    Product a b -> solveExp a * solveExp b