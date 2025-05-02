import Data.List (findIndices)

main :: IO ()
main = do
    contents <- readFile "25.txt"
    let [(a,b),(c,d)] = filter (\x -> snd x `elem` map read (lines contents)) $ zip [0..] $ take 20201228 $ iterate mix 1
    -- part 1
    print $ b^c `mod` 20201227
    -- part 2
    -- gg!

mix x = 7*x `mod` 20201227