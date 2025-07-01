module Day2017_01 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/01.txt"
    let add = print . sum . map (read . pure . fst) . filter (uncurry (==))
    -- part 1
    add $ (\x -> zip x (tail $ cycle x)) contents
    -- part 2
    add $ (\x -> zip x (drop (length x `div` 2) $ cycle x)) contents
