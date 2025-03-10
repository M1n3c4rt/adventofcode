import Data.Maybe (fromJust)
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "16.txt"
    let formatted = reverse $ dropWhile (=='0') $ reverse $ hexToBin $ takeWhile (/='\n') contents
    let packet = fst $ parsePacket formatted
    -- part 1
    print $ foldVersions packet
    -- part 2
    print $ evaluate packet

data Packet = Literal (Int,Int,Int) | Operator (Int,Int,[Packet]) deriving Show

hexToBin :: String -> String
hexToBin = concatMap (fromJust . (`lookup` l))
    where l = zip (['0'..'9'] ++ ['A'..'F']) ["0000","0001","0010","0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111"]

binToDec :: String -> Int
binToDec = sum . zipWith (*) (map (2^) [0..]) . map (read . pure) . reverse

parsePacket :: String -> (Packet,String)
parsePacket p =
    let version = binToDec $ take 3 p
        tID = binToDec $ take 3 $ drop 3 p
    in if tID == 4 then
            let (parsed,rest) = parseLiteral $ drop 6 p in (Literal (version,tID,binToDec parsed),rest)
        else
            let (parsed,rest) = parseOperator $ drop 6 p in (Operator (version,tID,parsed),rest)

parseLiteral :: String -> (String,String)
parseLiteral p = case splitAt 5 p of
    ('1':ss,rest) -> Data.Bifunctor.first (ss++) $ parseLiteral rest
    ('0':ss,rest) -> (ss,rest)

parseOperator :: String -> ([Packet],String)
parseOperator ('0':ss) =
    let (l,rest) = splitAt 15 ss
        l' = binToDec l
        helper n rem
            | n >= l' = ([],rem)
            | otherwise = let (parsed,r) = parsePacket rem in Data.Bifunctor.first (parsed:) $ helper (n + length rem - length r) r
    in helper 0 rest

parseOperator ('1':ss) =
    let (l,rest) = splitAt 11 ss
        l' = binToDec l
        helper n rem
            | n >= l' = ([],rem)
            | otherwise = let (parsed,r) = parsePacket rem in Data.Bifunctor.first (parsed:) $ helper (n+1) r
    in helper 0 rest

foldVersions :: Packet -> Int
foldVersions (Literal (a,_,_)) = a
foldVersions (Operator (a,_,ps)) = a + sum (map foldVersions ps)

evaluate :: Packet -> Int
evaluate (Literal (_,_,n)) = n
evaluate (Operator (_,t,us)) = let ns = map evaluate us in case t of
    0 -> sum ns
    1 -> product ns
    2 -> minimum ns
    3 -> maximum ns
    5 -> fromEnum $ head ns >  last ns
    6 -> fromEnum $ head ns <  last ns
    7 -> fromEnum $ head ns == last ns