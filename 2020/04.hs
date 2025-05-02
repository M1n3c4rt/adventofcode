import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.Char (isDigit)

main :: IO ()
main = do
    contents <- readFile "04.txt"
    -- part 1
    print $ length $ filter validate $ parse contents
    -- part 2
    print $ length $ filter validate' $ parse contents

parse :: String -> [HM.HashMap String String]
parse = map (HM.mapKeys init . HM.fromList . map (splitAt 4) . words) . splitOn "\n\n"

validate :: HM.HashMap String String -> Bool
validate p = HM.size p == 8 || ("cid" `notElem` HM.keys p && HM.size p == 7)

validate' :: HM.HashMap String String -> Bool
validate' p = (HM.size p == 8 || ("cid" `notElem` HM.keys p && HM.size p == 7)) &&
    and [
        let n = read $ HM.lookupDefault "0" "byr" p in n >= 1920 && n <= 2002,
        let n = read $ HM.lookupDefault "0" "iyr" p in n >= 2010 && n <= 2020,
        let n = read $ HM.lookupDefault "0" "eyr" p in n >= 2020 && n <= 2030,
        let h = HM.lookupDefault "0" "hgt" p; n = read $ filter isDigit h; u = filter (not . isDigit) h in if u == "cm" then n >= 150 && n <= 193 else n >= 59 && n <= 76,
        let (c:h) = HM.lookupDefault "0" "hcl" p in c == '#' && all (`elem` "0123456789abcdef") h,
        let c = HM.lookupDefault "0" "ecl" p in c `elem` ["amb","blu","brn","gry","grn","hzl","oth"],
        let n = HM.lookupDefault "0" "pid" p in length n == 9 && all isDigit n
    ]
