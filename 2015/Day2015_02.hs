module Day2015_02 where
import Utility.AOC (numbers, choose)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/02.txt"
    -- part 1
    print $ sum . map ((\l -> sum (map (2*) l) + minimum l) . map product . choose 2 . numbers) $ lines contents
    -- part 2
    print $ sum . map ((\l -> product l + 2 * minimum (map sum $ choose 2 l)) . numbers) $ lines contents