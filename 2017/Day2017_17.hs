module Day2017_17 where
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/17.txt"
    let s = foldl (spinlock 3) (HM.singleton 0 0) [0..2016]
    -- part 1
    print $ fromJust $ HM.lookup 2017 $ foldl (spinlock $ read contents) (HM.singleton 0 0) [0..2016]
    --print $ map fromJust $ take 2018 $ iterate ((!!4) . iterateM (`HM.lookup` s)) $ Just 0
    -- part 2
    print $ spinlock' 50000000 (read contents) 0 0 1

type Buffer = HM.HashMap Int Int

spinlock :: Int -> Buffer -> Int -> Buffer
spinlock n buffer k = HM.insert stepped (k+1) $ HM.insert (k+1) next buffer
    where
        Just stepped = iterateM (`HM.lookup` buffer) (Just k) !! n
        Just next = HM.lookup stepped buffer

spinlock' 0 _ afterZero _ _ = afterZero
spinlock' rounds step afterZero focus size
    | newFocus == 1 = spinlock' (rounds-1) step size newFocus (size+1)
    | otherwise = spinlock' (rounds-1) step afterZero newFocus (size+1)
    where newFocus = ((focus + step) `mod` size) + 1

iterateM f x = x : iterateM f (x >>= f)