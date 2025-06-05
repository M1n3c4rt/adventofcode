module Day2020_12 where


main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/12.txt"
    -- part 1
    print $ (\((a,b),_) -> abs a + abs b) $ foldl move ((0,0),(1,0)) $ lines contents
    -- part 2
    print $ (\((a,b),_) -> abs a + abs b) $ foldl move' ((0,0),(10,1)) $ lines contents

move ((x,y),(p,q)) (i:inst) = case i of
    'N' -> ((x,y+n),(p,q))
    'E' -> ((x+n,y),(p,q))
    'S' -> ((x,y-n),(p,q))
    'W' -> ((x-n,y),(p,q))
    'R' -> ((x,y),(p*cos' t + q*sin' t,q*cos' t - p*sin' t))
    'L' -> ((x,y),(p*cos' t - q*sin' t,q*cos' t + p*sin' t))
    'F' -> ((x+p*n,y+q*n),(p,q))
    where
        n = read inst
        t = pi/180 * fromIntegral n
        cos' = round . cos
        sin' = round . sin

move' ((x,y),(p,q)) (i:inst) = case i of
    'N' -> ((x,y),(p,q+n))
    'E' -> ((x,y),(p+n,q))
    'S' -> ((x,y),(p,q-n))
    'W' -> ((x,y),(p-n,q))
    'R' -> ((x,y),(p*cos' t + q*sin' t,q*cos' t - p*sin' t))
    'L' -> ((x,y),(p*cos' t - q*sin' t,q*cos' t + p*sin' t))
    'F' -> ((x+p*n,y+q*n),(p,q))
    where
        n = read inst
        t = pi/180 * fromIntegral n
        cos' = round . cos
        sin' = round . sin