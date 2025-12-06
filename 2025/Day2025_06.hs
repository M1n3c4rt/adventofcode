module Day2025_06 where
import Data.List (transpose)
import Data.Maybe (isNothing)
import Data.Char (isDigit)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/06.txt"
    -- part 1
    print $ sum $ map (\ls -> (if last ls == "+" then sum else product) (map read $ init ls)) $ transpose $ map words $ lines contents
    -- part 2
    print $ sum $ map (\(Just f,xs) -> f xs) $ bungle (Nothing,[]) $ transpose $ lines contents

bungle :: (Maybe ([Int] -> Int),[Int]) -> [String] -> [(Maybe ([Int] -> Int),[Int])]
bungle (op,cur) (c:cols) = if all (==' ') c then (op,cur) : bungle (Nothing,[]) cols else bungle (if isNothing op then case last c of
    '+' -> Just sum
    '*' -> Just product
    _ -> Nothing else op,read (filter isDigit c) : cur) cols
bungle (op,cur) [] = [(op,cur)]