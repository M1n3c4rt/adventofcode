import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, (\\), intersect, sort)

main :: IO ()
main = do
    contents <- readFile "08.txt"
    -- part 1
    print $ part1 $ parse contents
    -- part 2
    print $ sum $ map getNumber $ parse contents

parse :: String -> [([String], [String])]
parse = map ((\ws -> (take 10 ws, drop 11 ws)) . words) . lines

part1 :: [([String], [String])] -> Int
part1 = length . concatMap (filter (\s -> length s `elem` [2,3,4,7]) . snd)

solve :: [String] -> String
solve ss =
    let sorted = sortOn length ss
        [one,seven,four,f1,f2,f3,s1,s2,s3,eight] = sorted
        a = head $ seven \\ one
        b = head $ foldl1 intersect [four,s1,s2,s3] \\ one
        c = head $ one \\ [f]
        d = head $ four \\ (one ++ [b])
        e = head $ eight \\ [a,b,c,d,f,g]
        f = head $ filter (\p -> (==9) $ length $ filter (p `elem`) sorted) "abcdefg"
        g = head $ filter (\p -> all (p `elem`) [f1,f2,f3]) "abcdefg" \\ [a,b,c,d]
    in [a,b,c,d,e,f,g]

key :: HM.HashMap String Int
key =
    HM.fromList [
        ("abcefg",0),
        ("cf",1),
        ("acdeg",2),
        ("acdfg",3),
        ("bcdf",4),
        ("abdfg",5),
        ("abdefg",6),
        ("acf",7),
        ("abcdefg",8),
        ("abcdfg",9)
    ]

decode :: String -> String -> Int
decode pass text =
    let lookup = HM.fromList $ zip pass "abcdefg"
        transformed = sort $ map (\c -> HM.lookupDefault 'a' c lookup) text
    in HM.lookupDefault 0 transformed key

decodeM :: String -> [String] -> Int
decodeM pass = sum . zipWith (*) (map (10^) [0..]) . reverse . map (decode pass)

getNumber :: ([String], [String]) -> Int
getNumber (p,k) = decodeM (solve p) k