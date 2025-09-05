module Day2015_23 where
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/23.txt"
    -- part 1
    print $ fromJust $ HM.lookup 'b' $ run (HM.fromList $ zip [0..] $ lines contents) 0 (HM.fromList $ zip "ab" [0,0])
    -- part 2
    print $ fromJust $ HM.lookup 'b' $ run (HM.fromList $ zip [0..] $ lines contents) 0 (HM.fromList $ zip "ab" [1,0])

run :: HM.HashMap Int String -> Int -> HM.HashMap Char Int -> HM.HashMap Char Int
run insts i regs = case HM.lookup i insts of
    Nothing -> regs
    Just s -> case words s of
        ["hlf",x:_] -> run insts (i+1) $ HM.insert x (read' [x] `div` 2) regs
        ["tpl",x:_] -> run insts (i+1) $ HM.insert x (read' [x] * 3) regs
        ["inc",x:_] -> run insts (i+1) $ HM.insert x (read' [x] + 1) regs
        ["jmp",x] -> run insts (i+read' x) regs
        ["jie",y,x] -> run insts (i+if even (read' y) then read' x else 1) regs
        ["jio",y,x] -> run insts (i+if (==1) (read' y) then read' x else 1) regs
    where
        read' n = case readMaybe (filter (/='+') n) of
            Just x -> x
            Nothing -> fromJust $ HM.lookup (head n) regs