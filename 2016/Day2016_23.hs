module Day2016_23 where
import qualified Data.HashMap.Internal.Strict as HM
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/23.txt"
    -- part 1
    print $ fromJust $ HM.lookup 'a' $ run (HM.fromList $ zip [0..] $ lines contents) 0 $ HM.fromList (zip "abcd" [7,0,0,0])
    -- part 2 (inspected puzzle input)
    print $ factorial 12 + 5400

run :: HM.HashMap Int String -> Int -> HM.HashMap Char Int -> HM.HashMap Char Int
run insts i regs = case HM.lookup i insts of
    Nothing -> regs
    Just s -> case words s of
        ["cpy",x,y:_] -> run insts (i+1) $ if y `elem` "abcd" then HM.insert y (read' x) regs else regs
        ["inc",x:_] -> run insts (i+1) $ HM.adjust (+1) x regs
        ["dec",x:_] -> run insts (i+1) $ HM.adjust (subtract 1) x regs
        ["jnz",x,y] -> run insts (if read' x == 0 then i+1 else i+read' y) regs
        ["tgl",x] -> run (HM.adjust tgl (i+read' x) insts) (i+1) regs 
    where
        read' n = case readMaybe n of
            Just x -> x
            Nothing -> fromJust $ HM.lookup (head n) regs
        tgl' "inc" = "dec"
        tgl' "dec" = "inc"
        tgl' "cpy" = "jnz"
        tgl' "jnz" = "cpy"
        tgl' "tgl" = "inc"
        tgl s = let (x:xs) = words s in unwords (tgl' x:xs)

factorial x = product [1..x]

mysteryFunction :: String -> Int -> Int
mysteryFunction contents x = fromJust $ HM.lookup 'a' $ run (HM.fromList $ zip [0..] $ lines contents) 0 $ HM.fromList (zip "abcd" [x,0,0,0])