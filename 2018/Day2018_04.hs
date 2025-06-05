module Day2018_04 where
import Data.Char (isDigit)
import Data.List (sortOn, maximumBy, group, sort)
import qualified Data.HashMap.Strict as HM
import Data.Function (on)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/04.txt"
    -- part 1
    print $ (\(g,l) -> g * (head . last . sortOn length . group . sort) l) $ maximumBy (compare `on` (length . snd)) $ HM.toList $ times 0 $ sortOn fst $ map parse $ lines contents
    -- part 2
    print $ uncurry (*) $ fst $ maximumBy (compare `on` snd) $ HM.toList $ times' 0 $ sortOn fst $ map parse $ lines contents

parse :: String -> (Int,Int)
parse line = (read $ filter isDigit $ take 18 line,case filter isDigit $ drop 18 line of
    xs@(x:rest) -> read xs
    [] -> if 'w' `elem` line then 1 else 0)

times :: Int -> [(Int,Int)] -> HM.HashMap Int [Int]
times guard ((x,m):rest@((y,n):ys)) = case m of
    0 -> HM.insertWith (++) guard [x `mod` 100 .. (y `mod` 100) - 1] $ times guard ys
    k -> times k rest
times _ [] = HM.empty

times' :: Int -> [(Int,Int)] -> HM.HashMap (Int,Int) Int
times' guard ((x,m):rest@((y,n):ys)) = case m of
    0 -> foldr (\c acc -> HM.insertWith (+) (guard,c) 1 acc) (times' guard ys) [x `mod` 100 .. (y `mod` 100) - 1]
    k -> times' k rest
times' _ [] = HM.empty