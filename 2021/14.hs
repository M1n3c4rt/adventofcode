import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.MemoUgly (memo)

main :: IO ()
main = do
    contents <- readFile "14.txt"
    let (seed,ms) = parse contents
    -- part 1
    print $ getAnswer $ HM.insertWith (+) (head seed) 1 $ foldr1 (HM.unionWith (+)) $ map (\p -> expandMemo (ms,10,p)) $ pairs seed
    -- part 2
    print $ getAnswer $ HM.insertWith (+) (head seed) 1 $ foldr1 (HM.unionWith (+)) $ map (\p -> expandMemo (ms,40,p)) $ pairs seed

parse :: String -> (String, HM.HashMap String Char)
parse contents =
    let [seed, ms] = splitOn "\n\n" contents
        ms' = map ((\[a,b] -> (a,head b)) . splitOn " -> ") $ lines ms
    in (seed, HM.fromList ms')

expand :: (HM.HashMap String Char,Int,[Char]) -> HM.HashMap Char Int
expand (ms,0,[a,b]) = HM.singleton b 1
expand (ms,n,[a,c]) =
    let b = HM.lookupDefault 'a' [a,c] ms
    in HM.unionWith (+) (expandMemo (ms,n-1,[a,b])) (expandMemo (ms,n-1,[b,c]))

expandMemo :: (HM.HashMap String Char, Int, [Char]) -> HM.HashMap Char Int
expandMemo = memo expand

pairs :: [a] -> [[a]]
pairs (a:b:ss) = [a,b]:pairs (b:ss)
pairs [b] = []

getAnswer :: (Num a, Foldable t, Ord a) => t a -> a
getAnswer ms = maximum ms - minimum ms