module Day2016_16 where

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/16.txt"
    -- part 1
    print $ (!!4) $ iterate checksum $ take 272 $ dragonCurve contents
    -- part 2
    print $ (!!21) $ iterate checksum $ take 35651584 $ dragonCurve contents

fl :: Char -> Char
fl '0' = '1'
fl '1' = '0'

dragonCurve :: String -> String
dragonCurve a = a ++ "0" ++ drop (length a + 1) (dragonCurve $ a ++ "0" ++ map fl (reverse a))

checksum :: String -> String
checksum (a:b:bs) = (if a == b then '1' else '0') : checksum bs
checksum [] = []

iterateMaybe :: (t -> Maybe t) -> t -> [t]
iterateMaybe f x = case f x of
    Just x' -> x : iterateMaybe f x'
    Nothing -> [x]