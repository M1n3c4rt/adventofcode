import Data.List (transpose, sort, groupBy, elemIndex, elemIndices)
import Data.MemoUgly (memo)
import Data.Foldable (foldr')

main :: IO ()
main = do
    contents <- readFile "14.txt"
    let grid = rotateCW $ lines contents
    -- part 1
    print $ load $ map move grid
    -- part 2
    print $ spinload [] grid

applyn :: (b -> b) -> Int -> b -> b
applyn f 0 a = a
applyn f n a = f $! applyn f (n-1) a

move :: [Char] -> [Char]
move = concatMap sort . groupBy (\a b -> (a == '#') == (b == '#'))

rotateCW :: [[a]] -> [[a]]
rotateCW = transpose . reverse
rotateCCW :: [[a]] -> [[a]]
rotateCCW = reverse . transpose

spinload :: [[[Char]]] -> [[Char]] -> Int
spinload prev grid
--    | grid `elem` prev = let Just offset = elemIndex grid prev in let period = length $ dropWhile (/=grid) prev in load $ prev !! ((1000000000-offset) `mod` period)
    | grid `elem` prev = let offset = subtract 2 $ length $ dropWhile (/=grid) prev in let period = (+1) $ length $ takeWhile (/=grid) prev in load $ prev !! (offset `mod` period - 1000000000 `mod` period)
    | otherwise = spinload (grid:prev) (spincycle grid)

spinload'' :: [[[Char]]] -> [[Char]] -> [[[Char]]]
spinload'' prev grid
--    | grid `elem` prev = let Just offset = elemIndex grid prev in let period = length $ dropWhile (/=grid) prev in load $ prev !! ((1000000000-offset) `mod` period)
    | grid `elem` prev = grid:prev
    | otherwise = spinload'' (grid:prev) (spincycle grid)

spinload' :: [[Char]] -> [Int]
spinload' grid = load grid:spinload' (spincycle grid)

load :: [[Char]] -> Int
load = sum . concatMap (zipWith (*) [1..] . map (\c -> if c == 'O' then 1 else 0))

spincycle :: [[Char]] -> [[Char]]
spincycle = applyn (rotateCW . map move) 4