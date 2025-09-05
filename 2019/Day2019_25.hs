module Day2019_25 where
import Data.Char (ord)
import Intcode ( CompilerState(..), parse, runC, outputChr )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/25.txt"
    -- part 1
    let s = runC [] $ parse contents
    loop s
    -- part 2

loop :: CompilerState -> IO ()
loop s@(NeedsInput (a,b,c,d)) = do
    putStrLn $ outputChr s
    inp <- getLine
    loop (runC (map ord inp ++ [10]) s)
loop s@(Finished xs) = putStrLn $ outputChr s