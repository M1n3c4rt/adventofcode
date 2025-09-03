module Day2015_13 where
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (permutations)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/13.txt"
    -- part 1
    let table = HM.map HM.fromList $ HM.fromListWith (++) $ map parse $ lines contents
        (p:people) = HM.keys table
    print $ maximum $ map (score table . (p:)) (permutations people)
    -- part 2
    print $ maximum $ map (score table . ("":)) (permutations (p:people))

parse line = case words line of
    [a,_,"gain",n,_,_,_,_,_,_,b] -> (a,[(init b,read n)])
    [a,_,"lose",n,_,_,_,_,_,_,b] -> (a,[(init b,-read n)])

score :: HM.HashMap String (HM.HashMap String Int) -> [String] -> Int
score table arr = sum $ map (\(a,b) -> HM.lookupDefault 0 b $ HM.lookupDefault HM.empty a table) $ (\l -> l ++ map (uncurry (flip (,))) l) $ zip arr (tail arr ++ [head arr])