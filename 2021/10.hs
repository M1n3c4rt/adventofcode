import Data.Maybe (mapMaybe)
import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "10.txt"
    -- part 1
    print $ sum $ map (parse [-1]) $ lines contents
    -- part 2
    print $ getMiddle $ sort $ map getAutocompleteScore $ mapMaybe (parse' [-1]) $ lines contents

parse :: [Int] -> String -> Int
parse a@(s:ss) (q:qs) = case q of
    '(' -> parse (1:a) qs
    '[' -> parse (2:a) qs
    '{' -> parse (3:a) qs
    '<' -> parse (4:a) qs
    ')' -> if s == 1 then parse ss qs else 3
    ']' -> if s == 2 then parse ss qs else 57
    '}' -> if s == 3 then parse ss qs else 1197
    '>' -> if s == 4 then parse ss qs else 25137
parse _ [] = 0

parse' :: [Int] -> String -> Maybe [Int]
parse' a@(s:ss) (q:qs) = case q of
    '(' -> parse' (1:a) qs
    '[' -> parse' (2:a) qs
    '{' -> parse' (3:a) qs
    '<' -> parse' (4:a) qs
    ')' -> if s == 1 then parse' ss qs else Nothing
    ']' -> if s == 2 then parse' ss qs else Nothing
    '}' -> if s == 3 then parse' ss qs else Nothing
    '>' -> if s == 4 then parse' ss qs else Nothing
parse' a [] = Just a

getAutocompleteScore :: [Int] -> Int
getAutocompleteScore ns = foldl (\acc c -> 5*acc + c) 0 $ init ns

getMiddle :: [a] -> a
getMiddle ls = ls !! (length ls `div` 2)