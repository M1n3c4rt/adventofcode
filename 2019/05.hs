import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "05.txt"
    -- part 1
    print $ last $ run [1] 0 $ parse contents
    -- part 2
    print $ last $ run [5] 0 $ parse contents

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . zip [0..] . map read . splitOn ","

-- dear future minecart: i am so sorry you have to refactor this
run :: [Int] -> Int -> HM.HashMap Int Int -> [Int]
run inputs pointer state = case inst of
    1 -> run inputs (pointer+4) $ HM.insert c' (a+b) state
    2 -> run inputs (pointer+4) $ HM.insert c' (a*b) state
    3 -> run (tail inputs) (pointer+2) $ HM.insert a' (head inputs) state
    4 -> a : run inputs (pointer+2) state
    5 -> run inputs (if a == 0 then pointer+3 else b) state
    6 -> run inputs (if a /= 0 then pointer+3 else b) state
    7 -> run inputs (pointer+4) $ HM.insert c' (if a < b then 1 else 0) state
    8 -> run inputs (pointer+4) $ HM.insert c' (if a == b then 1 else 0) state
    99 -> []
    where
        Just (x,y,z,inst) = splitMode <$> HM.lookup pointer state

        Just a' = HM.lookup (pointer+1) state
        a = if z == 1 then a' else fromJust $ HM.lookup a' state
        Just b' = HM.lookup (pointer+2) state
        b = if y == 1 then b' else fromJust $ HM.lookup b' state
        Just c' = HM.lookup (pointer+3) state
        c = if x == 1 then c' else fromJust $ HM.lookup c' state

splitMode :: Integral d => d -> (d, d, d, d)
splitMode n = (n `div` 10000, (n `mod` 10000) `div` 1000, (n `mod` 1000) `div` 100, n `mod` 100)

run' :: [Int] -> String -> [Int]
run' ns = run ns 0 . parse