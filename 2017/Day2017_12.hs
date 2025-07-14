module Day2017_12 where
import qualified Data.HashMap.Strict as HM
import Utility.AOC (floodFill)
import Data.Maybe (fromJust)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/12.txt"
    -- part 1
    print $ S.size $ floodFill (fromJust . (`HM.lookup` HM.fromList (map parse (lines contents)))) (S.singleton 0) S.empty
    -- part 2
    print $ length $ groups $ HM.fromList $ map parse $ lines contents

parse :: String -> (Int, [Int])
parse line = let (a:_:ns) = words line in (read a,map (read . filter (/=',')) ns)

groups network = if HM.null network then [] else g:groups newNetwork
    where
        root = fst $ head $ HM.toList network
        g = floodFill (fromJust {-w-}. (`HM.lookup` network)) (S.singleton root) S.empty
        newNetwork = HM.filterWithKey (\k _ -> k `S.notMember` g) network