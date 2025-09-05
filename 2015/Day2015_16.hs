module Day2015_16 where
import qualified Data.HashMap.Strict as HM
import Utility.AOC (numbers)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/16.txt"
    -- part 1
    print $ fst $ head $ filter (isSue . snd) $ map parse $ lines contents
    -- part 2
    print $ fst $ head $ filter (isSue' . snd) $ map parse $ lines contents

tape :: HM.HashMap String Int
tape = HM.fromList [
        ("children",3),
        ("cats",7),
        ("samoyeds",2),
        ("pomeranians",3),
        ("akitas",0),
        ("vizslas",0),
        ("goldfish",5),
        ("trees",3),
        ("cars",2),
        ("perfumes",1)
    ]

parse :: String -> (Int, [(String,Int)])
parse l = let (a:ns) = numbers l; (_:ws) = map init $ filter ((==':') . last) $ words l in (a,zip ws ns)

isSue :: [(String, Int)] -> Bool
isSue = all (\(k,v) -> HM.lookup k tape == Just v)
isSue' :: [(String, Int)] -> Bool
isSue' = all (\(k,v) -> if k `elem` ["cats","trees"] then (v>) $ fromJust $ HM.lookup k tape else if k `elem` ["pomeranians","goldfish"] then (v<) $ fromJust $ HM.lookup k tape else HM.lookup k tape == Just v)