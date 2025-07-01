module Day2017_05 where
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/05.txt"
    -- part 1
    print $ jump 0 $ HM.fromList $ zip [0..] $ map read $ lines contents
    -- part 2
    print $ jump' 0 $ HM.fromList $ zip [0..] $ map read $ lines contents

jump :: Int -> HM.HashMap Int Int -> Int
jump ptr insts = case HM.lookup ptr insts of
    Just n -> 1 + jump (ptr+n) (HM.adjust (+1) ptr insts)
    Nothing -> 0

jump' :: Int -> HM.HashMap Int Int -> Int
jump' ptr insts = case HM.lookup ptr insts of
    Just n -> (1+) $! jump' (ptr+n) (HM.adjust (if n < 3 then (+1) else subtract 1) ptr insts)
    Nothing -> 0