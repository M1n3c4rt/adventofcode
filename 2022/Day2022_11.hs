module Day2022_11 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Ord (Down(Down), comparing)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/11.txt"
    let ms = parse contents
    let n = getLcm contents
    -- part 1
    print $ product $ take 2 $ sortBy (comparing Down) (pass True n 20 ms)
    -- part 2
    print $ product $ take 2 $ sortBy (comparing Down) (pass False n 10000 ms)

type Monkeys = HM.HashMap Integer Monkey
type Monkey = (Integer, [Integer], Integer -> Integer, Integer -> Integer)

parse :: String -> Monkeys
parse = HM.fromList . map (helper . lines) . splitOn "\n\n"
    where helper [a,b,c,d,e,f] = (
                read [last $ init a],
                (
                    0,
                    read $ "[" ++ dropWhile (not . isDigit) b ++ "]",
                    let [x,y,z] = drop 3 $ words c in (\old -> (if y == "*" then (*) else (+)) (if x == "old" then old else read x) (if z == "old" then old else read z)),
                    let [p,q,r] = map (read . last . words) [d,e,f] in (\x -> if x `mod` p == 0 then q else r)
                )
            )

getLcm :: String -> Integer
getLcm = product . map (read . last . words . (!!3) . lines) . splitOn "\n\n"

pass :: Bool -> Integer -> Integer -> Monkeys -> [Integer]
pass r l 0 old = map (\(a,b,c,d) -> a) $ HM.elems old
pass r l n old = pass r l (n-1) $ subpass 0 old
    where subpass n m = case HM.lookup n m of
                            Just monkey -> subpass (n+1) (helper n monkey m)
                            Nothing -> m
          helper n (passes, i:items, transform, test) m =
            let newi = (if r then (`div` 3) else (`mod` l)) $ transform i
                next = test newi
                newm' = HM.adjust (\(a,b,c,d) -> (a,newi:b,c,d)) next m
                newm = HM.adjust (\(a,b,c,d) -> (a+1,tail b,c,d)) n newm'
            in helper n (passes, items, transform, test) newm
          helper n (_,[],_,_) m = m