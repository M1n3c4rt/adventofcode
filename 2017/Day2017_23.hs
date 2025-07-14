module Day2017_23 where
import qualified Data.HashMap.Strict as HM
import Text.Read (readMaybe)
import Debug.Trace (traceShow)
import Utility.AOC (traceSleepSeconds, traceSleep)
import Data.Numbers.Primes (isPrime)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/23.txt"
    -- part 1
    --print $ runInsts 0 HM.empty $ HM.fromList $ zip [0..] $ map parse $ lines contents
    -- part 2
    print $ length $ filter (not . isPrime) [108100,108117..125100]
    --print $ runInsts 0 (HM.singleton 'a' 1) $ HM.fromList $ zip [0..] $ map parse $ lines contents

data Value = Reg Char | Number Int

parse :: String -> (String, Value, Value)
parse l = case words l of
    [a,b] -> (a,readN b,Number 0)
    [a,b,c] -> (a,readN b, readN c)
    where
        readN s = case readMaybe s of
            Just k -> Number k
            Nothing -> Reg (head s)

runInsts :: Int -> HM.HashMap Char Int -> HM.HashMap Int (String,Value,Value) -> Int
runInsts pointer regs insts = traceShow (pointer,regs) . traceSleep (if pointer == 20 then 5000000 else 50000) $ case HM.lookup pointer insts of
    Nothing -> 0
    Just inst -> case inst of
        ("set",a@(Reg r),b) -> runInsts (pointer+1) (HM.insert r (f b) regs) insts
        ("sub",a@(Reg r),b) -> runInsts (pointer+1) (HM.insert r (f a - f b) regs) insts
        ("mul",a@(Reg r),b) -> 1 + runInsts (pointer+1) (HM.insert r (f a * f b) regs) insts
        ("jnz",a,b) -> if f a /= 0 then runInsts (pointer+f b) regs insts else runInsts (pointer+1) regs insts
    where
        f (Number n) = n
        f (Reg r) = HM.lookupDefault 0 r regs