import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.MemoUgly (memo)

main :: IO ()
main = do
    contents <- readFile "19.txt"
    let [rs,ss] = splitOn "\n\n" contents
        rules = let old = HM.fromList $ map parse $ lines rs in HM.map (\v -> (v,getLength v old)) old
        zero = fromJust $ HM.lookup 0 rules
        [fortyTwo, thirtyOne] = map (\x -> fromJust $ HM.lookup x rules) [42,31]
    -- part 1
    print $ length $ filter (\s -> eat s zero rules) $ lines ss
    -- part 2
    print $ length $ filter (\s -> eatInf s (fortyTwo,thirtyOne) 0 rules) $ lines ss

data Rule = Char Char | Single Int | Or Rule Rule | Combo Int Int deriving (Show, Eq, Ord)

parse :: String -> (Int, Rule)
parse line = (read start,rule)
    where
        (start,rest) = span (/=':') line
        rule
            | '"' `elem` rest = Char (rest !! 3)
            | length ws == 2 = Single $ read $ last ws
            | '|' `elem` rest =
                if length ws == 4 then
                    (\[a,b] -> Or (Single (read a)) (Single (read b))) $ map (ws !!) [1,3]
                else
                    (\[a,b,c,d] -> Or (Combo (read a) (read b)) (Combo (read c) (read d))) $ map (ws !!) [1,2,4,5]
            | otherwise = (\[a,b] -> Combo (read a) (read b)) $ map (words rest !!) [1,2]
            where ws = words rest

getLength :: Rule -> HM.HashMap Int Rule -> Int
getLength rule rules = case rule of
    Char x -> 1
    Single n -> getLength (fromJust $ HM.lookup n rules) rules
    Or r1 r2 -> getLength r1 rules
    Combo m n -> getLength (fromJust $ HM.lookup m rules) rules + getLength (fromJust $ HM.lookup n rules) rules

eat :: String -> (Rule,Int) -> HM.HashMap Int (Rule,Int) -> Bool
eat ss (rule,k) rules = length ss == k && case rule of
    Char x -> head ss == x
    Single n -> eat ss (fromJust $ HM.lookup n rules) rules
    Or r1 r2 -> eat ss (r1,k) rules || eat ss (r2,k) rules
    Combo m n -> let ((r1,k1),(r2,k2)) = (fromJust $ HM.lookup m rules,fromJust $ HM.lookup n rules) in eat (take k1 ss) (r1,k1) rules && eat (drop k1 ss) (r2,k2) rules

eatInf :: [Char] -> ((Rule, Int), (Rule, Int)) -> Int -> HM.HashMap Int (Rule, Int) -> Bool
eatInf "" _ c1 rules = False
eatInf ss ((r1,k1),(r2,k2)) c1 rules
    | c1 < 2 = length ss >= k1 && eat (take k1 ss) (r1,k1) rules && eatInf (drop k1 ss) ((r1,k1),(r2,k2)) (c1+1) rules
    | otherwise =
        (eat (take k1 ss) (r1,k1) rules && eatInf (drop k1 ss) ((r1,k1),(r2,k2)) (c1+1) rules) ||
        (length ss `mod` k2 == 0 && length ss `div` k2 < c1 && all (\part -> eat part (r2,k2) rules) (takeEvery k2 ss))
    where
        takeEvery n [] = []
        takeEvery n ls = let (a,b) = splitAt n ls in a:takeEvery n b