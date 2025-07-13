module Day2017_18 where
import Text.Read (readMaybe)
import qualified Data.HashMap.Strict as HM
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/18.txt"
    let insts = HM.fromList $ zip [0..] $ map parse $ lines contents
    -- part 1
    print $ runInsts 0 HM.empty insts
    -- part 2
    -- the answer is the number of o's in the output string before it throws an exception (i'm sorry)
    print $ duet insts

data Value = Number Int | Reg Char deriving Show

parse :: String -> (String, Value, Value)
parse l = case words l of
    [a,b] -> (a,readN b,Number 0)
    [a,b,c] -> (a,readN b, readN c)
    where
        readN s = case readMaybe s of
            Just k -> Number k
            Nothing -> Reg (head s)

runInsts :: Int -> HM.HashMap Char Int -> HM.HashMap Int (String,Value,Value) -> Int
runInsts pointer regs insts = case inst of
    ("snd",a,b) -> runInsts (pointer+1) (HM.insert ' ' (f a) regs) insts
    ("set",a@(Reg r),b) -> runInsts (pointer+1) (HM.insert r (f b) regs) insts
    ("add",a@(Reg r),b) -> runInsts (pointer+1) (HM.insert r (f a + f b) regs) insts
    ("mul",a@(Reg r),b) -> runInsts (pointer+1) (HM.insert r (f a * f b) regs) insts
    ("mod",a@(Reg r),b) -> runInsts (pointer+1) (HM.insert r (f a `mod` f b) regs) insts
    ("rcv",a,b) -> if f a == 0 then runInsts (pointer+1) regs insts else HM.lookupDefault 0 ' ' regs
    ("jgz",a,b) -> if f a > 0 then runInsts (pointer+f b) regs insts else runInsts (pointer+1) regs insts
    where
        Just inst = HM.lookup pointer insts
        f (Number n) = n
        f (Reg r) = HM.lookupDefault 0 r regs

runInsts' :: [Int] -> Int -> HM.HashMap Char Int -> HM.HashMap Int (String,Value,Value) -> [Int]
runInsts' inputs pointer regs insts = case HM.lookup pointer insts of
    Just inst -> case inst of
        ("snd",a,b) -> f a : runInsts' inputs (pointer+1) (HM.insert ' ' (f a) regs) insts
        ("set",a@(Reg r),b) -> runInsts' inputs (pointer+1) (HM.insert r (f b) regs) insts
        ("add",a@(Reg r),b) -> runInsts' inputs (pointer+1) (HM.insert r (f a + f b) regs) insts
        ("mul",a@(Reg r),b) -> runInsts' inputs (pointer+1) (HM.insert r (f a * f b) regs) insts
        ("mod",a@(Reg r),b) -> runInsts' inputs (pointer+1) (HM.insert r (f a `mod` f b) regs) insts
        ("rcv",a@(Reg r),b) -> runInsts' (tail inputs) (pointer+1) (HM.insert r (head inputs) regs) insts
        ("jgz",a,b) -> if f a > 0 then runInsts' inputs (pointer+f b) regs insts else runInsts' inputs (pointer+1) regs insts
    Nothing -> []
    where
        f (Number n) = n
        f (Reg r) = HM.lookupDefault 0 r regs

duet :: HM.HashMap Int (String, Value, Value) -> String
duet insts = map (const 'o') b
    where
        a = runInsts' b 0 (HM.singleton 'p' 0) insts
        b = runInsts' a 0 (HM.singleton 'p' 1) insts