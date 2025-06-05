module Day2023_08 where

import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/08.txt"
    let (dirs, m) = parse contents
    -- part 1
    print $ length $ takeWhile (/= "ZZZ") $ takeStep m dirs "AAA"
    -- part 2
    print $ foldl lcm 1 $ map (length . takeWhile (((/='Z') . (!!2)) . snd) . zip [0..] . takeStep m dirs) (filter ((=='A') . (!!2)) $ HM.keys m)

parse :: String -> (String, HM.HashMap String (String, String))
parse = (\x -> (cycle $ head x, HM.fromList $ map (\line -> (take 3 line, (take 3 $ drop 7 line, take 3 $ drop 12 line))) $ drop 2 x)) . lines

takeStep :: HM.HashMap String (String, String) -> String -> String -> [String]
takeStep m (d:dirs) cur = (cur:) $ takeStep m dirs next
    where next = (if d == 'L' then fst else snd) $ HM.lookupDefault ("","") cur m

getPeriod :: [String] -> Int -> HM.HashMap String (String, String) -> String -> String -> Int
getPeriod ends o m dirs cur = let path = takeStep m dirs cur in head $ [ x-o | x <- [o+1..], path !! o == path !! (x+o)]

getOffset :: [String] -> HM.HashMap String (String, String) -> [Char] -> [Char] -> Int
getOffset ends m dirs cur = let path = takeStep m dirs cur in head $ [x | x <- [0..], path !! x `elem` ends]

getBoth :: [String] -> HM.HashMap String (String, String) -> [Char] -> [[Char]] -> [(Int, Int)]
getBoth ends m dirs = map (\cur -> let o = getOffset ends m dirs cur in (getPeriod ends o m dirs cur, o))