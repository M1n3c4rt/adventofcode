module Day2018_12 where
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/12.txt"
    let (start,table) = setify $ parse $ lines contents
    -- part 1
    print $ sum $ (!!20) $ iterate (step table) start
    -- part 2
    let d = (!!200) $ diffList $ map sum $ iterate (step table) start
        a = sum $ (!!200) $ iterate (step table) start
    print $ a + (50000000000-200)*d

diffList (x:y:ys) = (y-x):diffList (y:ys)
diffList [x] = []

parse :: [String] -> (String, HM.HashMap String Char)
parse (l:ls) = (drop 15 l,HM.fromList $ map (\l' -> (take 5 l', l' !! 9)) $ tail ls)

setify :: (String, HM.HashMap String Char) -> (S.Set Int, HM.HashMap (S.Set Int) Bool)
setify (s,ss) = (plants s, HM.map (=='#') $ HM.mapKeys (S.map (+(-2)) . plants) ss)
    where plants = S.fromList . map fst . filter ((=='#') . snd) . zip [0..]

step :: HM.HashMap (S.Set Int) Bool -> S.Set Int -> S.Set Int
step table plants = S.filter isNext range
    where
        range = S.fromList [minimum plants - 2 .. maximum plants + 2]
        isNext p = HM.lookupDefault False (S.filter ((`S.member` plants) . (p+)) $ S.fromList [-2..2]) table