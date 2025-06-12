module Day2018_16 where
import Data.Bits ((.&.), Bits ((.|.)))
import Data.Maybe (isNothing, mapMaybe, fromJust)
import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/16.txt"
    -- part 1
    print $ length $ filter ((>=3) . length) $ map (\(xs,(r,a,b,c),ys) -> getPossible xs (a,b,c) ys) $ parseTop contents
    -- part 2
    let table = resolve [] $ map (\(xs,(r,a,b,c),ys) -> (r,getPossible xs (a,b,c) ys)) $ parseTop contents
    print $ head $ foldl (run table) [0,0,0,0] $ parseBottom contents

parseTop :: String -> [([Int],(Int,Int,Int,Int),[Int])]
parseTop = map ((\[a,b,c] -> (read $ drop 8 a, (\[r,x,y,z] -> (r,x,y,z)) $ map read $ words b, read $ drop 8 c)) . lines) . splitOn "\n\n" . head . splitOn "\n\n\n"

parseBottom :: String -> [(Int,Int,Int,Int)]
parseBottom = map ((\[a,b,c,d] -> (a,b,c,d)) . map read . words) . lines . last . splitOn "\n\n\n\n"

getPossible :: [Int] -> (Int, Int, Int) -> [Int] -> [String]
getPossible rs@[x,y,z,w] (a,b,c) target = map fst $ filter ((==target) . snd) $ allOps rs (a,b,c)
    where
        insertAt 0 (x:xs) y = y:xs
        insertAt n (x:xs) y = x:insertAt (n-1) xs y

run :: [(Int, String)] -> [Int] -> (Int, Int, Int, Int) -> [Int]
run table rs (r,a,b,c) = fromJust $ lookup (fromJust $ lookup r table) $ allOps rs (a,b,c)

allOps :: [Int] -> (Int, Int, Int) -> [(String, [Int])]
allOps rs (a,b,c) = map (second (insertAt c rs)) [
        ("addr",a'+b'),
        ("addi",a'+b),
        ("mulr",a'*b'),
        ("muli",a'*b),
        ("banr",a'.&.b'),
        ("bani",a'.&.b),
        ("borr",a'.|.b'),
        ("bori",a'.|.b),
        ("setr",a'),
        ("seti",a),
        ("gtir",fromEnum $ a>b'),
        ("gtri",fromEnum $ a'>b),
        ("gtrr",fromEnum $ a'>b'),
        ("eqir",fromEnum $ a==b'),
        ("eqri",fromEnum $ a'==b),
        ("eqrr",fromEnum $ a'==b')
    ]
    where
        [a',b',c'] = map (rs!!) [a,b,c]
        insertAt 0 (x:xs) y = y:xs
        insertAt n (x:xs) y = x:insertAt (n-1) xs y

resolve :: [(Int,[String])] -> [(Int,[String])] -> [(Int,String)]
resolve [] [] = []
resolve unfinished [] = resolve [] unfinished
resolve unfinished (r@(n,pos):remaining)
    | null pos = resolve unfinished remaining
    | length pos == 1 = let p = head pos in (n,p) : resolve (reduce p unfinished) (reduce p remaining)
    | otherwise = resolve (r:unfinished) remaining
    where reduce x = map (second (filter (/=x)))