import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Data.MemoUgly (memo)

main :: IO ()
main = do
    handle <- openFile "19.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    let (patterns,towels) = getTowels contents in putStr $ show $ length $ filter id $ map (\x -> getPatternMemo (patterns,x)) towels
    putStr "\n"
    -- part 2
    let (patterns,towels) = getTowels contents in putStr $ show $ sum $ map (\x -> getPatternsMemo (patterns,x)) towels
    putStr "\n"
    hClose handle

type Pattern = String
type Towel = String

getTowels :: String -> ([Pattern],[Towel])
getTowels s = let [a,b] = splitOn "\n\n" s in (splitOn ", " a, lines b)

getPatternMemo :: ([Pattern], Towel) -> Bool
getPatternMemo = memo getPattern

getPattern :: ([Pattern],Towel) -> Bool
getPattern (ps,[]) = True
getPattern (ps,towel) = any (\p -> getPatternMemo (ps,drop (length p) towel)) (filter (`startsWith` towel) ps)

getPatternsMemo :: ([Pattern], Towel) -> Int
getPatternsMemo = memo getPatterns

getPatterns :: ([Pattern],Towel) -> Int
getPatterns (ps,[]) = 1
getPatterns (ps,towel) = sum $ map (\p -> getPatternsMemo (ps,drop (length p) towel)) (filter (`startsWith` towel) ps)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith y x = take (length y) x == y