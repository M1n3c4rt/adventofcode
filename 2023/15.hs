import Data.Char (ord)
import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "15.txt"
    -- part 1
    print $ sum $ map hash $ splitOn "," contents
    -- part 2
    print $ total $ foldl add (HM.fromList $ map (,[]) [0..255]) $ splitOn "," contents

type Boxes = HM.HashMap Int [(String,Int)]

hash :: String -> Int
hash = foldl (\n s -> ((n + ord s) * 17) `mod` 256) 0

add :: Boxes -> String -> Boxes
add boxes inst
    | '-' `elem` inst = HM.adjust (remove $ init inst) (hash $ init inst) boxes
    | otherwise = let (label, power) = (takeWhile (/='=') inst,read [last inst]) in HM.adjust (insert label power) (hash label) boxes
    where remove label = filter ((/=label) . fst)
          insert label power l = if not (any ((==label) . fst) l) then (label,power):l else
            map (\(label',power') -> if label==label' then (label,power) else (label',power')) l

total :: Boxes -> Int
total = HM.foldl (+) 0 . HM.mapWithKey (\b l -> (*(b+1)) $ sum $ zipWith (\(l,p) n -> p*n) l [length l,length l-1..1])