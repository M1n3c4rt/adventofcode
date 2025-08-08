module Day2016_10 where
import Utility.AOC (numbers)
import qualified Data.HashMap.Lazy as HM
import Data.Either (rights, lefts, fromRight)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/10.txt"
    -- part 1
    let botRange = maximum $ rights $ concatMap parse $ lines contents
    let outputRange = maximum $ lefts $ concatMap parse $ lines contents
    print $ fromRight 0 $ head $ HM.keys $ HM.filter (==[17,61]) $ botMap botRange outputRange $ map parse $ lines contents
    -- part 2
    print $ product $ concat $ fromJust $ (\x -> mapM ((`HM.lookup` x) . Left) [0,1,2]) $ botMap botRange outputRange $ map parse $ lines contents

parse :: String -> [Either Int Int]
parse line = case numbers line of
    [a,b] -> [Left a,Right b]
    [a,b,c] -> let [_,x,y] = filter (`elem` ["bot","output"]) $ words line in
        [Right a,(if x == "output" then Left else Right) b,(if y == "output" then Left else Right) c]


botMap :: Int -> Int -> [[Either Int Int]] -> HM.HashMap (Either Int Int) [Int]
botMap bs os insts = final
    where
        final = HM.fromList $ map helper (map Right [0..bs] ++ map Left [0..os])
        helper x = (x,) $ foldr (extract x) [] insts
        extract (Right x) [Left a,Right b] acc = if b == x then a:acc else acc
        extract (Left _) [_,_] acc = acc
        extract x [Right c,a,b] acc
            | x == a = minimum cs : acc
            | x == b = maximum cs : acc
            | otherwise = acc
            where Just cs = HM.lookup (Right c) final