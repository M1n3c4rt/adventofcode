module Day2019_07 where

import Data.List (permutations)
import Intcode ( CompilerState(Finished), parse, runC, getOutput, isFinished )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/07.txt"
    -- part 1
    print $ maximum $ map (\x -> runMult 0 x $ parse contents) $ permutations [0,1,2,3,4]
    -- part 2
    print $ maximum $ map (\x -> runMult' x $ parse contents) $ permutations [5,6,7,8,9]

runMult :: Int -> [Int] -> CompilerState -> Int
runMult prev inputs state = foldl (\prev' i -> head $ (\(Finished (a,b,c,d)) -> d) $ runC [i,prev'] state) prev inputs

runMult' :: [Int] -> CompilerState -> Int
runMult' [i1,i2,i3,i4,i5] state =
    let updateOutputs ins states = if all isFinished states then states else
            let newStates = zipWith runC ins states
                newIns = take 5 $ drop 4 $ cycle $ map getOutput newStates
            in updateOutputs newIns newStates
    in last $ getOutput $ last $ updateOutputs [[i1,0],[i2],[i3],[i4],[i5]] $ repeat state