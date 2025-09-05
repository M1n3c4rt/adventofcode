{-# LANGUAGE LambdaCase #-}
module Day2016_17 where
import Data.ByteString.Char8 (unpack, pack)
import Data.ByteString.Base16 (encode)
import Crypto.Hash.MD5 (hash)
import Data.Bifunctor (bimap)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/17.txt"
    -- part 1
    putStrLn $ reverse $ diffString $ floodFill contents [[(0,0)]]
    -- part 2
    print $ floodFill' contents [[(0,0)]] (0,0)

inBounds :: (Int, Int) -> Bool
inBounds (x,y) = x >= 0 && x < 4 && y >= 0 && y < 4

diffString ((x,y):path) = let diff = zipWith (\(a,b) (c,d) -> (a-c,b-d)) ((x,y):path) path in
    map (
        \case
            (0,-1) -> 'U'
            (0,1) -> 'D'
            (-1,0) -> 'L'
            (1,0) -> 'R'
        ) diff

neighbours :: String -> [(Int, Int)] -> [(Int, Int)]
neighbours seed ((x,y):path) = filter inBounds $ map (bimap (x+) (y+) . snd) $ filter fst $ zip ns [(0,-1),(0,1),(-1,0),(1,0)]
    where
        ns = map (`elem`"bcdef") $ take 4 $ unpack $ encode $ hash $ pack $ seed ++ reverse (diffString ((x,y):path))

floodFill :: String -> [[(Int, Int)]] -> [(Int,Int)]
floodFill seed paths = case filter ((==(3,3)) . head) paths of
    [x] -> x
    _ -> floodFill seed (concatMap helper paths)
    where
        helper path = map (:path) (neighbours seed path)

floodFill' :: String -> [[(Int, Int)]] -> (Int,Int) -> Int
floodFill' seed paths (buffer,acc)
    | all end paths = buffer-1
    | otherwise = floodFill' seed (concatMap helper $ filter (not . end) paths) (new,acc+1)
    where
        helper path = map (:path) (neighbours seed path)
        end = (==(3,3)) . head
        new = if any end paths then acc+1 else buffer