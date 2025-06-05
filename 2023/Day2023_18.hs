module Day2023_18 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/18.txt"
    -- part 1
    print $ enclosedArea $ convertToPoints $ parse1 contents
    -- part 2
    print $ enclosedArea $ convertToPoints $ parse2 contents

parse1 :: String -> [(Char, Int)]
parse1 = map ((\[a,b,_] -> (head a,read b)) . words) . lines

parse2 :: String -> [(Char, Int)]
parse2 = map ((\[_,_,c] -> ("RDLU" !! read [c !! 7], read $ ("0x"++) $ take 5 $ drop 2 c)) . words) . lines

convertToPoints :: [(Char, Int)] -> [(Int, Int)]
convertToPoints = foldl helper [(0,0)]
    where helper a@((x,y):acc) ('U',n) = (x,y+n):a
          helper a@((x,y):acc) ('R',n) = (x+n,y):a
          helper a@((x,y):acc) ('L',n) = (x-n,y):a
          helper a@((x,y):acc) ('D',n) = (x,y-n):a

enclosedArea :: [(Int, Int)] -> Int
enclosedArea points = round $ b + (a+1-(b/2))
    where a = area points
          b = fromIntegral $ boundary points

boundary :: Num b => [(b, b)] -> b
boundary ((x,y):(p,q):points) = abs (x-p) + abs (y-q) + boundary ((p,q):points)
boundary [(x,y)] = 0

area :: (Fractional a1, Integral a2) => [(a2, a2)] -> a1
area points = (/2) $ fromIntegral $ sum (zipWith (*) (map fst $ init points) (map snd $ tail points)) - sum (zipWith (*) (map fst $ tail points) (map snd $ init points))