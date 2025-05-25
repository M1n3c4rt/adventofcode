module Intcode where

import qualified Data.HashMap.Strict as HM
import Data.Char (chr)
import Data.List.Split (splitOn)

data CompilerState = NeedsInput (Int,Int,HM.HashMap Int Int,[Int]) | Finished (Int,Int,HM.HashMap Int Int,[Int])

parse :: String -> CompilerState
parse = (\x -> NeedsInput (0,0,x,[])) . HM.fromList . zip [0..] . map read . splitOn ","

cons :: Int -> CompilerState -> CompilerState
cons x (NeedsInput (a,b,c,xs)) = NeedsInput (a,b,c,x:xs)
cons x (Finished (a,b,c,xs)) = Finished (a,b,c,x:xs)

modify :: Int -> Int -> CompilerState -> CompilerState
modify k v (NeedsInput (a,b,c,d)) = NeedsInput (a,b,HM.insert k v c,d)
modify _ _ s = s

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
    99 -> Finished (pointer,relBase,state,[])
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

runC :: [Int] -> CompilerState -> CompilerState
runC inputs s@(NeedsInput (a,b,c,d)) = run inputs a b c
runC _ s = s

getOutput :: CompilerState -> [Int]
getOutput (NeedsInput (a,b,c,d)) = d
getOutput (Finished (a,b,c,d)) = d

outputChr :: CompilerState -> String
outputChr = map chr . getOutput

getState :: CompilerState -> HM.HashMap Int Int
getState (NeedsInput (a,b,c,d)) = c
getState (Finished (a,b,c,d)) = c

isFinished :: CompilerState -> Bool
isFinished (NeedsInput _) = False
isFinished (Finished _) = True