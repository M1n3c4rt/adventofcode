module Day2018_21 where
import Data.Maybe (fromJust)
import Day2018_16 (allOps)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/21.txt"
    let (loc,insts) = parses $ lines contents
    let vs = run loc 0 insts [69,0,0,0,0,0]
    -- part 1
    print $ head vs
    -- part 2
    print $ getLastUnique S.empty vs

getLastUnique :: Ord a => S.Set a -> [a] -> a
getLastUnique finished (x:y:ys)
    | y `S.member` finished = x
    | otherwise = getLastUnique (S.insert x finished) (y:ys)

parses :: [String] -> (Int,[(String,[Int])]) 
parses (l:ls) = (read $ drop 4 l,map parse ls)
    where parse line = let (a,b) = splitAt 4 line in (a,map read $ words b)

run :: Int -> Int -> [(String, [Int])] -> [Int] -> [Int]
run loc pointer insts regs@[a,b,c,d,e,f] = if pointer >= length insts then [] else
    let (inst,[x,y,z]) = insts !! pointer
        newRegs = if inst == "muli" && y == 256 then let (p,q) = d `divMod` 256 in [a,256*(p+1),p,d,e,f] else fromJust $ lookup inst $ allOps regs (x,y,z)
        newPointer = ((newRegs !! loc) + 1)
    in (if inst == "eqrr" then (regs !! 4:) else id) $ run loc newPointer insts $ take loc newRegs ++ [newPointer] ++ drop (loc+1) newRegs