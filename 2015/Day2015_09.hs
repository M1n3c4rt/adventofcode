module Day2015_09 where
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.List.Extra (permutations)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/09.txt"
    -- part 1
    let table = HM.map HM.fromList $ HM.fromListWith (++) $ concatMap dist $ lines contents
    print $ minimum $ map (`dists` table) $ permutations (HM.keys table)
    -- part 2
    print $ maximum $ map (`dists` table) $ permutations (HM.keys table)

dist :: String -> [(String, [(String, Int)])]
dist l = let [a,_,c,_,e] = words l in [(c,[(a,read e)]),(a,[(c,read e)])]

dists :: [String] -> HM.HashMap String (HM.HashMap String Int) -> Int
dists ns table = sum $ catMaybes $ catMaybes $ zipWith (\a b -> HM.lookup b <$> HM.lookup a table) ns (tail ns)