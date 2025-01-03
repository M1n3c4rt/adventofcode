import Data.MemoUgly (memo)
import Data.Bits ( Bits(shift, xor) )
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "22.txt"
    -- part 1
    print $ sum $ map (getNextSecretNMemo 2000 . read) (lines contents)
    -- part 2
    let digits = map (getNextSecretDigitNMemo 2000 . read) (lines contents)
    print $ sum $ map (getBananaFromPattern $ mode $ mergeSeqMaps $ map getSeqs digits) digits

getNextSecretNMemo :: Int -> Int -> Int
getNextSecretNMemo = memo getNextSecretN

getNextSecretN :: Int -> Int -> Int
getNextSecretN 0 a = a
getNextSecretN n a = getNextSecretNMemo (n-1) $ getNextSecret a

getBananaFromPattern ::  (Int,Int,Int,Int) -> [Int] -> Int
getBananaFromPattern (a,b,c,d) (n:o:p:q:r:rs) 
    | (o-n,p-o,q-p,r-q) == (a,b,c,d) = r
    | otherwise = getBananaFromPattern (a,b,c,d) (o:p:q:r:rs) 
getBananaFromPattern (a,b,c,d) (o:p:q:r:rs)  = 0

getSeqs :: [Int] -> HM.HashMap (Int,Int,Int,Int) Int
getSeqs (n:o:p:q:r:rs) = HM.insert (o-n,p-o,q-p,r-q) r $ getSeqs (o:p:q:r:rs)
getSeqs (o:p:q:r:rs) = HM.empty

mergeSeqMaps :: [HM.HashMap (Int,Int,Int,Int) Int] -> HM.HashMap (Int,Int,Int,Int) Int
mergeSeqMaps (l:m:ms) = mergeSeqMaps $ HM.unionWith (+) l m : ms
mergeSeqMaps [m] = m

mode :: HM.HashMap (Int,Int,Int,Int) Int -> (Int,Int,Int,Int)
mode l = fst $ head $ HM.toList $ HM.filter (==HM.foldl max 0 l) l

getNextSecret :: Int -> Int
getNextSecret a = let f = (`mod` (2^24)) $ xor a $ shift a 6 in
    let g = (`mod` (2^24)) $ xor f $ shift f (-5) in
        (`mod` (2^24)) $ xor g $ shift g 11

getNextSecretDigitNMemo :: Int -> Int -> [Int]
getNextSecretDigitNMemo = memo getNextSecretDigitN

getNextSecretDigitN :: Int -> Int -> [Int]
getNextSecretDigitN 0 a = [a `mod` 10]
getNextSecretDigitN n a = a `mod` 10:getNextSecretDigitNMemo (n-1) (getNextSecretDigit a)

getNextSecretDigit :: Int -> Int
getNextSecretDigit a = let f = (`mod` (2^24)) $ xor a $ shift a 6 in
    let g = (`mod` (2^24)) $ xor f $ shift f (-5) in
        (`mod` (2^24)) $ xor g $ shift g 11