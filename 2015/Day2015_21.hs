module Day2015_21 where
import Utility.AOC (choose, numbers)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/21.txt"
    -- part 1
    let boss = let [a,b,c] = numbers contents in (a,(0,b,c))
    print $ minimum $ map (\(a,b,c) -> a) $ filter (winner . iterate battle . (,boss) . (100,)) combos
    -- part 2
    print $ maximum $ map (\(a,b,c) -> a) $ filter (not . winner . iterate battle . (,boss) . (100,)) combos

weapons = [
        (8,4,0),
        (10,5,0),
        (25,6,0),
        (40,7,0),
        (74,8,0)
    ]

armor = [
        (13,0,1),
        (31,0,2),
        (53,0,3),
        (75,0,4),
        (102,0,5)
    ]

rings = [
        (25,1,0),
        (50,2,0),
        (100,3,0),
        (20,0,1),
        (40,0,2),
        (80,0,3)
    ]

combos = [foldr (\(a,b,c) (d,e,f) -> (a+d,b+e,c+f)) (0,0,0) (w:a++r) | w <- weapons, a <- []:map pure armor, r <- []:map pure rings++choose 2 rings]

battle ((ph,(pg,pa,pd)),(bh,(bg,ba,bd))) =
    let playerAttack = bh-max 1 (pa-bd)
        bossAttack = ph-max 1 (ba-pd)
    in ((bossAttack,(pg,pa,pd)),(playerAttack,(bg,ba,bd)))

winner (((ph,_),(bh,_)):bs)
    | bh <= 0 = True
    | ph <= 0 = False
    | otherwise = winner bs