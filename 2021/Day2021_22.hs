module Day2021_22 where

import Data.Char (isDigit)
import Data.List (groupBy)
import Data.Function (on)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2021/22.txt"
    -- part 1
    print $ sum $ map volume $ foldAreas $ filter (isNotEmpty . restrict . snd) $ parse contents
    -- part 2
    print $ sum $ map volume $ foldAreas $ parse contents

type Area = ((Int,Int,Int),(Int,Int,Int))

parse :: String -> [(Bool,Area)]
parse = map (\l -> (l!!1 == 'n',) $ (\[a,d,b,e,c,f] -> ((a,b,c),(d,e,f))) $ map read $ [(!!1),(!!3),(!!5),(!!7),(!!9),(!!11)] <*> pure (groupBy ((==) `on` isNum) l)) . lines
    where isNum c = isDigit c || c == '-'

isOverlapping :: Area -> Area -> Bool
isOverlapping ((a,b,c),(d,e,f)) ((p,q,r),(x,y,z)) = all helper [((a,d),(p,x)),((b,e),(q,y)),((c,f),(r,z))]
    where helper ((x1,x2),(x3,x4)) = (x2 >= x3) && (x1 <= x4)

isNotEmpty :: Area -> Bool
isNotEmpty ((a,b,c),(d,e,f)) = d >= a && e >= b && f >= c

(*-*) :: Area -> Area -> [Area]
a1@((a,b,c),(d,e,f)) *-* a2@((p',q',r'),(x',y',z'))
    | isOverlapping a1 a2 = let ((p,q,r),(x,y,z)) = ((max a p', max b q', max c r'),(min d x', min e y', min f z')) in filter isNotEmpty
        [
            ((a,b,c),                                        (d,e,r-1)),

            ((a,b,r),                                        (d,q-1,z)),
            ((a,q,r),(p-1,y,z)),                    ((x+1,q,r),(d,y,z)),
            ((a,y+1,r),                                        (d,e,z)),

            ((a,b,z+1),                                        (d,e,f))
        ]
    | otherwise = [a1]

(**-*) :: [Area] -> Area -> [Area]
as **-* a = concatMap (*-* a) as

(**+*) :: [Area] -> Area -> [Area]
as **+* a = a:as **-* a

restrict :: Area -> Area
restrict ((a,b,c),(d,e,f)) = let x = -50; y = 50 in ((max x a,max x b,max x c),(min y d, min y e, min y f))

foldAreas :: [(Bool,Area)] -> [Area]
foldAreas = foldl (\acc (b,c) -> if b then acc **+* c else acc **-* c) []

volume :: Area -> Int
volume a1@((a,b,c),(d,e,f)) = if isNotEmpty a1 then (d-a+1)*(e-b+1)*(f-c+1) else 0