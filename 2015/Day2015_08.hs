module Day2015_08 where
import Numeric (readHex)
import Data.Char (chr)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/08.txt"
    -- part 1
    print $ sum $ map (\x -> 2 + length x - length (reduce x)) $ lines contents
    -- part 2
    print $ sum $ map (\x -> 2 + length (unreduce x) - length x) $ lines contents

reduce :: String -> String
reduce ('\\':'\\':s) = '\\':reduce s
reduce ('\\':'"':s) = '"':reduce s
reduce ('\\':'x':a:b:s) = chr (fst $ head $ readHex [a,b]):reduce s
reduce (x:s) = x : reduce s
reduce [] = []

unreduce :: String -> String
unreduce ('\\':s) = "\\\\" ++ unreduce s
unreduce ('"':s) = "\\\"" ++ unreduce s
unreduce (x:s) = x : unreduce s
unreduce [] = []