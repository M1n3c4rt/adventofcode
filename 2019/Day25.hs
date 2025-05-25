module Day25 where
import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Char (chr, ord)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/25.txt"
    -- part 1
    let s = run [] 0 0 $ parse contents
    loop s
    -- part 2

loop s@(NeedsInput (a,b,c,d)) = do
    print s
    inp <- getLine
    loop (run (map ord inp ++ [10]) a b c)
loop s@(Finished xs) = print s

data CompilerState = NeedsInput (Int,Int,HM.HashMap Int Int,[Int]) | Finished [Int]

instance Show CompilerState where
    show (NeedsInput (a,b,c,d)) = "\n\n[NEEDS INPUT]" ++ map chr d
    show (Finished xs) = "\n\n[FINISHED]" ++ map chr xs

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

cons :: Int -> CompilerState -> CompilerState
cons x (Finished xs) = Finished (x:xs)
cons x (NeedsInput (a,b,c,xs)) = NeedsInput (a,b,c,x:xs)

run :: [Int] -> Int -> Int -> HM.HashMap Int Int -> CompilerState
run inputs pointer relBase state = case inst of
    1 -> run inputs (pointer+4) relBase $ HM.insert cW (a+b) state
    2 -> run inputs (pointer+4) relBase $ HM.insert cW (a*b) state
    3 -> if null inputs then NeedsInput (pointer,relBase,state,[]) else
        run (tail inputs) (pointer+2) relBase $ HM.insert aW (head inputs) state
    4 -> cons a $ run inputs (pointer+2) relBase state
    5 -> run inputs (if a == 0 then pointer+3 else b) relBase state
    6 -> run inputs (if a /= 0 then pointer+3 else b) relBase state
    7 -> run inputs (pointer+4) relBase $ HM.insert cW (if a < b then 1 else 0) state
    8 -> run inputs (pointer+4) relBase $ HM.insert cW (if a == b then 1 else 0) state
    9 -> run inputs (pointer+2) (relBase+a) state
    99 -> Finished []
    where
        (inst:mods) = splitMode $ HM.lookupDefault 0 pointer state

        vals@(a':b':c':vs) = map (\x -> HM.lookupDefault 0 (pointer+x) state) [1..]
        refs@(a :b :c :rs) = zipWith applyMod vals mods
        refWrites@(aW:bW:cW:rws) = zipWith applyModW vals mods

        applyMod x' m = case m of
            0 -> HM.lookupDefault 0 x' state
            1 -> x'
            2 -> HM.lookupDefault 0 (relBase + x') state

        applyModW x' m = case m of
            0 -> x'
            2 -> relBase + x'

splitMode :: (Integral a, Read a, Show a) => a -> [a]
splitMode n = n `mod` 100 : map (read . pure) (reverse $ show $ n `div` 100) ++ repeat 0