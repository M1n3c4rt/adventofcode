module Day2019_04 where

import Data.List (sort, group)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/04.txt"
    -- part 1
    print $ length $ filter validate $ getRange contents
    -- part 2
    print $ length $ filter validate' $ getRange contents

getRange :: String -> [Int]
getRange ss = [read $ take 6 ss .. read $ drop 7 ss]

validate :: Int -> Bool
validate n = sort ds == ds && any ((>=2) . length) (group ds)
    where
        ds :: [Int]
        ds = map (read . pure) $ show n

validate' :: Int -> Bool
validate' n = sort ds == ds && any ((==2) . length) (group ds)
    where
        ds :: [Int]
        ds = map (read . pure) $ show n