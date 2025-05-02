import Data.Char (ord)
import Data.List (find)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "03.txt"
    -- part 1
    print $ sum $ fromJust $ mapM (fmap toOrd . findCommon) (lines contents)
    -- part 2
    print $ sum $ fromJust $ mapM (fmap toOrd . findCommonThree) $ takeThrees $ lines contents

findCommon :: String -> Maybe Char
findCommon s = let (a,b) = splitAt (length s `div` 2) s in
    find (`elem` b) a

findCommonThree :: (String,String,String) -> Maybe Char
findCommonThree (a,b,c) = find (\x -> x `elem` b && x `elem` c) a

takeThrees :: [c] -> [(c, c, c)]
takeThrees [] = []
takeThrees (x:y:z:zs) = (x,y,z):takeThrees zs

toOrd :: Char -> Int
toOrd c
    | c > 'Z' = ord c - 96
    | otherwise = ord c - 38