import Data.List.Split (splitOn)
import qualified Data.HashMap.Internal.Strict as HM
import Data.Maybe (fromJust)
import Data.List (permutations, minimumBy)
import Data.Function (on)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    contents <- readFile "14.txt"
    let table = HM.fromList $ map parse $ lines contents
    let ore x = snd $ head $ HM.toList $ resolveRemainders table $ unfold table (x,"FUEL")
    -- part 1
    print $ ore 1
    -- part 2
    print $ binarySearch ore 1000000000000 (1,1000000000000)

parse :: [Char] -> (String,(Int,[(Int, String)]))
parse line =
    let [rs,p] = splitOn " => " line
        sep x = let [a,b] = words x in (read a,b)
        reagents = map sep $ splitOn ", " rs
        (count,product) = sep p
    in (product,(count,reagents))

unfold :: HM.HashMap String (Int, [(Int, String)]) -> (Int, String) -> HM.HashMap String Int
unfold table (amount,product)
    | product == "ORE" = HM.singleton product amount
    | otherwise =
        let Just (a,rs) = HM.lookup product table
            (q,r) = amount `divMod` a
        in (if r == 0 then id else HM.insert product r) $ foldr1 (HM.unionWith (+)) $ map (\(n,r) -> unfold table (q*n,r)) rs

isChildOf :: HM.HashMap String (Int, [(Int, String)])  -> String -> String -> Bool
isChildOf table a b
    | a == "ORE" = False
    | otherwise =
        let Just (m,rs) = HM.lookup a table
        in any (\(n,r) -> r == b || isChildOf table r b) rs

resolveRemainders :: HM.HashMap String (Int, [(Int, String)]) -> HM.HashMap String Int -> HM.HashMap String Int
resolveRemainders table remaining
    | HM.size remaining == 1 = remaining
    | otherwise =
    let next = HM.foldlWithKey helper HM.empty remaining
        helper a k v =
            if any (\x -> isChildOf table x k) $ HM.keys remaining then
                HM.insertWith (+) k v a
            else
                let Just (m,rs) = HM.lookup k table in HM.unionWith (+) (unfold table (m,k)) a
    in resolveRemainders table $ foldr1 (HM.unionWith (+)) $ map (\(ss,n) -> unfold table (n,ss)) $ HM.toList next

binarySearch f lim (rmin,rmax)
    | rmax - rmin == 1 = rmin
    | f ((rmin+rmax) `div` 2) < lim = binarySearch f lim ((rmin+rmax) `div` 2,rmax)
    | f ((rmin+rmax) `div` 2) > lim = binarySearch f lim (rmin,(rmin+rmax) `div` 2)