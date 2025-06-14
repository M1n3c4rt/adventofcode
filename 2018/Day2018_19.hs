module Day2018_19 where
import Day2018_16 (allOps)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/19.txt"
    let (loc,insts) = parses $ lines contents
    -- part 1
    print $ head $ run loc 0 insts [0,0,0,0,0,0]
    -- part 2
    print $ sum $ (\x -> concatMap (\y -> [y,x `div` y]) $ filter ((==0) . (x `mod`)) [1..round (sqrt (fromIntegral x))]) $ maximum $ run loc 0 (init insts ++ [("seti",[50,0,2])]) [1,0,0,0,0,0]

parses :: [String] -> (Int,[(String,[Int])]) 
parses (l:lines) = (read $ drop 4 l,map parse lines)
    where parse line = let (a,b) = splitAt 4 line in (a,map read $ words b)

run :: Int -> Int -> [(String, [Int])] -> [Int] -> [Int]
run loc pointer insts regs@[a,b,c,d,e,f] = if pointer >= length insts then regs else
    let (inst,[a,b,c]) = insts !! pointer
        newRegs = fromJust $ lookup inst $ allOps regs (a,b,c)
        newPointer = ((newRegs !! loc) + 1)
    in run loc newPointer insts $ take loc newRegs ++ [newPointer] ++ drop (loc+1) newRegs