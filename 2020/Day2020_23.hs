module Day2020_23 where

import Data.Char (isDigit)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HM
import Data.Foldable (Foldable(foldl'))

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/23.txt"
    let ls = map (read . return) $ filter isDigit contents
        ls' = ls ++ [10..1000000] :: [Int]
        first = head ls
    -- part 1
    putStrLn $ map (head . show) $ take 8 $ reconstruct 1 $ snd $ (!!100) $ iterate (move 9) $ (first,) $ HM.fromList $ zip ls (tail $ cycle ls)
    -- part 2
    print $ product $ take 2 $ reconstruct 1 $ snd $ foldl' (flip ($)) ((first,) $ HM.fromList $ zip ls' (tail $ cycle ls')) (replicate 10000000 (move 1000000))

move m (current,ls) =
    let x = fromJust $ HM.lookup current ls
        y = fromJust $ HM.lookup x ls
        z = fromJust $ HM.lookup y ls
        dest = lower [x,y,z] m current
        new =  HM.insert dest x $ HM.insert z (fromJust $ HM.lookup dest ls) $ HM.insert current (fromJust $ HM.lookup z ls) ls
    in (fromJust $ HM.lookup current new,new)

lower :: (Foldable t, Integral a) => t a -> a -> a -> a
lower xs m n = head $ filter (`notElem` xs) $ map ((+1) . (`mod` m) . subtract 1) $ tail $ iterate (subtract 1) n

reconstruct n ls = let next = fromJust $ HM.lookup n ls in next : reconstruct next ls