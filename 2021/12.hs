import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.List (nub)
import Data.Char (isUpper)

main :: IO ()
main = do
    contents <- readFile "12.txt"
    -- part 1
    print $ search ["start"] $ parse contents
    -- part 2
    print $ search' ["start"] $ parse contents

parse :: String -> HM.HashMap String [(String,Bool)]
parse contents =
    let raw = map (splitOn "-") $ lines contents
        caves = nub $ concat raw
        helper (c::String) = HM.insert c (map ((\cs -> (cs,isUpper (head cs))) . head . filter (/=c)) $ filter (c `elem`) raw)
    in foldr helper HM.empty caves

search :: [String] -> HM.HashMap String [(String, Bool)] -> Int
search (p:path) caves =
    let neighbours = HM.lookupDefault [] p caves
        allow (name,size) = name `notElem` path || size
    in if p == "end" then 1 else sum $ map (\(new,s) -> search (new:p:path) caves) $ filter allow neighbours

search' :: [String] -> HM.HashMap String [(String, Bool)] -> Int
search' (p:path) caves =
    let neighbours = HM.lookupDefault [] p caves
        allow' (name,size) = (name /= "start") && (length (filter (==name) path) == 1 && not size)
        allow (name,size) = name `notElem` path || size
    in if p == "end" then 1 else
        sum (map (\(new,s) -> search (new:p:path) caves) $ filter allow' neighbours) + sum (map (\(new,s) -> search' (new:p:path) caves) $ filter allow neighbours)