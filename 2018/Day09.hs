{-# LANGUAGE BangPatterns #-}
module Day09 where
import qualified Data.HashMap.Strict as HM
import Debug.Trace (traceShowId, traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/09.txt"
    let [players,rounds] = parse contents
    -- part 1
    print $ maximum $ play rounds players 0 0 0 HM.empty HM.empty
    -- part 2
    print $ maximum $ play (rounds*100) players 0 0 0 HM.empty HM.empty

parse :: String -> [Int]
parse line = map (read . (words line!!)) [0,6]

play :: Int -> Int -> Int -> Int -> Int -> HM.HashMap Int (Int,Int) -> HM.HashMap Int Int -> HM.HashMap Int Int
play l m player n current circle !scores
    | n == l = scores
    | HM.null circle = play l m 0 1 0 (HM.singleton 0 (0,0)) HM.empty
    | n `mod` 23 == 0 = play l m ((player+1) `mod` m) (n+1) sixBefore (removeBefore sixBefore circle) (HM.insertWith (+) player (n+sevenBefore) scores)
    | otherwise = play l m ((player+1) `mod` m) (n+1) n (insertAfter oneAfter n circle) scores
    where
        prev k = fst <$> HM.lookup k circle
        next k = snd <$> HM.lookup k circle
        Just oneAfter = next current
        Just sixBefore = (!!6) $ iterateM prev $ Just current
        Just sevenBefore = prev sixBefore

insertAfter :: Int -> Int -> HM.HashMap Int (Int, Int) -> HM.HashMap Int (Int, Int)
insertAfter n k circle =
    HM.insert k (n,next) $ HM.adjust (\(a,b) -> (a,k)) n $ HM.adjust (\(a,b) -> (k,b)) next circle
    where
        Just (prev,next) = HM.lookup n circle

removeBefore :: Int -> HM.HashMap Int (Int, Int) -> HM.HashMap Int (Int, Int)
removeBefore n circle =
    HM.delete prev $ HM.adjust (\(a,b) -> (a,n)) prev' $ HM.adjust (\(a,b) -> (n,b)) n circle
    where
        Just (prev,next) = HM.lookup n circle
        Just (prev',next') = HM.lookup prev circle

iterateM :: Monad m => (b -> m b) -> m b -> [m b]
iterateM f x = x : iterateM f (x >>= f)