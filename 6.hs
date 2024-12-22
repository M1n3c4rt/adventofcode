import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List ( sort, transpose, intercalate )

main :: IO ()
main = do
    handle <- openFile "6.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    putStr $ show $ sum $ map (numAppearances "1234") $ step $ rotateCW $ lines contents
    putStr "\n"
    -- part 2
    putStr $ show $ length $ filter (any ('E' `elem`)) $ map (step . replGuard' (rotateCW $ lines contents)) $ genLists' $ rotateCCW $ step $ rotateCW $ lines contents
    putStr "\n"
    hClose handle

replaceX :: String -> String
replaceX (s:ss)
    | all (`notElem` (s:ss)) "^BCD" = s:ss
    | s `elem` "#1234." = s:replaceX ss
    | (s `elem` "^BCD") && not (null ss) && (head ss == '#') = s:ss
    | (s `elem` "^") && not (null ss) && (head ss == '.') = "1" ++ replaceX ("^" ++ tail ss)
    | (s `elem` "^") && not (null ss) && (head ss == '1') = "1" ++ replaceX ("B" ++ tail ss)
    | (s `elem` "^") && not (null ss) && (head ss == '2') = "1" ++ replaceX ("C" ++ tail ss)
    | (s `elem` "^") && not (null ss) && (head ss == '3') = "1" ++ replaceX ("D" ++ tail ss)
    | (s `elem` "B") && not (null ss) && (head ss == '.') = "2" ++ replaceX ("^" ++ tail ss)
    | (s `elem` "B") && not (null ss) && (head ss == '1') = "2" ++ replaceX ("B" ++ tail ss)
    | (s `elem` "B") && not (null ss) && (head ss == '2') = "2" ++ replaceX ("C" ++ tail ss)
    | (s `elem` "B") && not (null ss) && (head ss == '3') = "2" ++ replaceX ("D" ++ tail ss)
    | (s `elem` "C") && not (null ss) && (head ss == '.') = "3" ++ replaceX ("^" ++ tail ss)
    | (s `elem` "C") && not (null ss) && (head ss == '1') = "3" ++ replaceX ("B" ++ tail ss)
    | (s `elem` "C") && not (null ss) && (head ss == '2') = "3" ++ replaceX ("C" ++ tail ss)
    | (s `elem` "C") && not (null ss) && (head ss == '3') = "3" ++ replaceX ("D" ++ tail ss)
    | (s `elem` "D") && not (null ss) && (head ss == '.') = "4" ++ replaceX ("^" ++ tail ss)
    | (s `elem` "D") && not (null ss) && (head ss == '1') = "4" ++ replaceX ("B" ++ tail ss)
    | (s `elem` "D") && not (null ss) && (head ss == '2') = "4" ++ replaceX ("C" ++ tail ss)
    | (s `elem` "D") && not (null ss) && (head ss == '3') = "4" ++ replaceX ("D" ++ tail ss)
    | (s `elem` "^BCD") && not (null ss) && (head ss == '4') = "1" ++ "E" ++ tail ss
    | s == '^' = "1"
    | s == 'B' = "2"
    | s == 'C' = "3"
    | s == 'D' = "4"
replaceX [] = []

rotateCCW :: [[a]] -> [[a]]
rotateCCW = reverse . transpose

rotateCW :: [[a]] -> [[a]]
rotateCW = map reverse . transpose

step :: [[Char]] -> [[Char]]
step l
    | any (any (`elem` "^BCD")) l = step $ rotateCCW $ map replaceX l
    | otherwise = l

numAppearances :: String -> String -> Int
numAppearances n l = length [a | a <- l, a `elem` n]

genLists :: String -> [String]
genLists (s:ss)
    | s `elem` "1234" = ('#':removeX ss):map ('.':) (genLists ss)
    | otherwise = map (s:) $ genLists ss
genLists "" = [""]

genLists' (l:ls) = map (:map removeX ls) (init $ genLists l) ++ map (removeX l:) (genLists' ls)
genLists' [] = [[]]

replGuard :: [Char] -> [Char] -> [Char]
replGuard (x:xs) (y:ys)
    | '^' == x = '^':ys
    | otherwise = y:replGuard xs ys

replGuard' :: [[Char]] -> [[Char]] -> [[Char]]
replGuard' (p:ps) (q:qs)
    | '^' `elem` p = replGuard p q:qs
    | otherwise = q:replGuard' ps qs

removeX (s:ss)
    | s `elem` "1234" = '.':removeX ss
    | otherwise = s:removeX ss
removeX [] = []