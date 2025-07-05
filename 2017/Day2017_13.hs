module Day2017_13 where
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/13.txt"
    -- part 1
    print $ severity (maximum $ HM.keys $ parse contents) $ parse contents
    -- part 2
    print $ head $ filter (\x -> isSafe x $ parse contents) [0..]

parse :: String -> HM.HashMap Int Int
parse = HM.fromList . map ((\[a,b] -> (read (init a),2*(read b-1))) . words) . lines

severity m ss = sum $ map helper [0..m]
    where
        helper x = case HM.lookup x ss of
            Nothing -> 0
            Just r -> if x `mod` r == 0 then x*((r `div` 2) + 1) else 0


isSafe offset ss = all (\(x,r) -> ((x+offset) `mod` r) /= 0) $ HM.toList ss