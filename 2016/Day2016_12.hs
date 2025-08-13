module Day2016_12 where
import qualified Data.HashMap.Strict as HM
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/12.txt"
    -- part 1
    print $ run (HM.fromList $ zip [0..] $ lines contents) 0 $ HM.fromList $ zip "abcd" [0,0,0,0]
    -- part 2
    print $ run (HM.fromList $ zip [0..] $ lines contents) 0 $ HM.fromList $ zip "abcd" [0,0,1,0]

run :: HM.HashMap Int String -> Int -> HM.HashMap Char Int -> HM.HashMap Char Int
run insts i regs = case HM.lookup i insts of
    Nothing -> regs
    Just s -> case words s of
        ["cpy",x,y:_] -> run insts (i+1) $ HM.insert y (read' x) regs
        ["inc",x:_] -> run insts (i+1) $ HM.adjust (+1) x regs
        ["dec",x:_] -> run insts (i+1) $ HM.adjust (subtract 1) x regs
        ["jnz",x,y] -> run insts (if read' x == 0 then i+1 else i+read' y) regs
    where
        read' n = case readMaybe n of
            Just x -> x
            Nothing -> fromJust $ HM.lookup (head n) regs