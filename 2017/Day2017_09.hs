module Day2017_09 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/09.txt"
    -- part 1
    print $ score 1 $ read contents
    -- part 2
    print $ count $ read contents

data Garbage = Group [Garbage] | Garbage String deriving Show

removeExcs :: String -> String
removeExcs ('!':s:ss) = removeExcs ss
removeExcs (s:ss) = s:removeExcs ss
removeExcs [] = []

readGroup :: String -> (Garbage, String)
readGroup (s:ss)
    | s == '<' = let (a,b) = span (/='>') ss in (Garbage a,tail b)
    | s == '{' = readGroups ss

readGroups :: String -> (Garbage, String)
readGroups ss = if not (null ss) && head ss == '}' then (Group [],tail ss) else case readGroup ss of
    (g,',':rest) -> let (Group gs,rest') = readGroups rest in (Group (g:gs),rest')
    (g,'}':rest) -> (Group [g],rest)

instance Read Garbage where
    readsPrec :: Int -> ReadS Garbage
    readsPrec _ s = [readGroup $ removeExcs s]

score :: Int -> Garbage -> Int
score prec (Group gs) = prec + sum (map (score (prec+1)) gs)
score _ (Garbage _) = 0

count :: Garbage -> Int
count (Garbage s) = length s
count (Group gs) = sum $ map count gs