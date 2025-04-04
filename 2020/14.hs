import qualified Data.HashMap.Strict as HM
import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Function (on)
import Debug.Trace (traceShowId, traceShow)

main :: IO ()
main = do
    contents <- readFile "14.txt"
    -- part 1
    print $ HM.foldr (+) 0 $ HM.map binToDec $ snd $ foldl updateState ("",HM.empty) $ lines contents
    -- part 2
    print $ HM.foldr (+) 0 $ HM.map binToDec $ snd $ foldl updateState' ("",HM.empty) $ lines contents

data Ternary = F | T | X deriving Enum
type State = (String,HM.HashMap Int [Ternary])

decToBin :: Integral t => t -> [Ternary]
decToBin y = take 36 $ helper y ++ repeat F
    where
        helper 0 = []
        helper x = let (q,r) = x `divMod` 2 in (if r == 1 then T else F):decToBin q

binToDec :: [Ternary] -> Int
binToDec = sum . zipWith (*) (map (2^) [0..]) . map fromEnum

applyMask :: [Ternary] -> String -> [Ternary]
applyMask (n:ns) (m:mask) = case m of
    '0' -> F:applyMask ns mask
    '1' -> T:applyMask ns mask
    'X' -> n:applyMask ns mask
applyMask [] [] = []

applyMask' :: [Ternary] -> String -> [[Ternary]]
applyMask' (n:ns) (m:mask) = case m of
    '0' -> map (n:) $ applyMask' ns mask
    '1' -> map (T:) $ applyMask' ns mask
    'X' -> concatMap (\c -> [F:c,T:c]) $ applyMask' ns mask
applyMask' [] [] = [[]]

updateState :: State -> String -> State
updateState (mask,mems) line
    | take 4 line == "mask" = (reverse (drop 7 line), mems)
    | otherwise = let [_,b,_,d] = groupBy ((==) `on` isDigit) line in (mask,HM.insert (read b) (applyMask (decToBin $ read d) mask) mems)

updateState' :: State -> String -> State
updateState' (mask,mems) line
    | take 4 line == "mask" = (reverse (drop 7 line), mems)
    | otherwise = let [_,b,_,d] = groupBy ((==) `on` isDigit) line in (mask,foldl (\acc c -> HM.insert (binToDec c) (decToBin $ read d) acc) mems $ applyMask' (decToBin $ read b) mask)