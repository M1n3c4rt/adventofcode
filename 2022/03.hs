import Data.Char (ord)

main :: IO ()
main = do
    contents <- readFile "03.txt"
    -- part 1
    print $ sum $ map (toOrd . findCommon) $ lines contents
    -- part 2
    print $ sum $ map (toOrd . findCommonThree) $ takeThrees $ lines contents

findCommon :: String -> Char
findCommon s = let (a,b) = splitAt (length s `div` 2) s in
    head $ filter (`elem` b) a

findCommonThree :: (String,String,String) -> Char
findCommonThree (a,b,c) = head $ filter (\x -> x `elem` b && x `elem` c) a

takeThrees :: [c] -> [(c, c, c)]
takeThrees [] = []
takeThrees (x:y:z:zs) = (x,y,z):takeThrees zs

toOrd :: Char -> Int
toOrd c
    | c > 'Z' = ord c - 96
    | otherwise = ord c - 38