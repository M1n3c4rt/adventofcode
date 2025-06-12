module Day2018_20 where
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Bifunctor (Bifunctor(first))
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/20.txt"
    let graph = fst $ toGraph $ fixParse $ parseRegex $ tail $ init contents
    -- part 1
    print $ getLongest 0 S.empty (S.singleton (0,0)) graph
    -- part 2
    print $ S.size $ getLongest' 0 S.empty (S.singleton (0,0)) graph

data Regex = Solo String | Option Regex Regex | Group [Regex] deriving (Show,Eq)

takeNextBracket :: Int -> String -> (String,String)
takeNextBracket n (s:ss)
    | s == '(' = let (a,b) = takeNextBracket (n+1) ss in (s:a,b)
    | s == ')' = if n == 1 then ([s],ss) else let (a,b) = takeNextBracket (n-1) ss in (s:a,b)
    | otherwise = let (a,b) = takeNextBracket n ss in (s:a,b)
takeNextBracket _ "" = ("","")

findSep :: Int -> String -> (String,String)
findSep n "" = ("","")
findSep n (s:ss)
    | n == 0 && s == '|' = ("",ss)
    | s == '(' = let (a,b) = findSep (n+1) ss in (s:a,b)
    | s == ')' = let (a,b) = findSep (n-1) ss in (s:a,b)
    | otherwise = let (a,b) = findSep n ss in (s:a,b)

parseRegex :: String -> Regex
parseRegex ss
    | null b && null after = Solo a
    | null after = case rest of
        Group r -> Group $ [Solo a,parseRegex $ tail $ init c] ++ r
        Solo s -> Group [Solo a,parseRegex $ tail $ init c,Solo d]
    | otherwise = Option (parseRegex before) (parseRegex after)
    where
        (before,after) = findSep 0 ss
        (a,b) = span (/='(') ss
        (c,d) = takeNextBracket 0 b
        rest = parseRegex d

fixParse :: Regex -> Regex
fixParse (Solo s) = if (not . null) s && last s == '|' then Option (Solo $ init s) (Solo "") else Solo s
fixParse (Group ss) = Group $ map fixParse $ filter (/=Solo "") ss
fixParse (Option a b) = Option (fixParse a) (fixParse b)

toGraph :: Regex -> (HM.HashMap (Int, Int) (S.Set (Int, Int)), S.Set (Int, Int))
toGraph (Solo (c:cs)) = let m = move c (0,0) in (first (HM.insertWith S.union (0,0) (S.singleton m)) $ shift m $ toGraph (Solo cs))
toGraph (Solo []) = (HM.empty,S.singleton (0,0))
toGraph (Option a b) = let (g1,t1) = toGraph a; (g2,t2) = toGraph b in (HM.unionWith S.union g1 g2,S.union t1 t2)
toGraph (Group (g:gs)) = let (g1,t1) = toGraph g; (g2,t2) = toGraph (Group gs) in foldr (\(p,q) (r,s) -> (HM.unionWith S.union p r, S.union q s)) (g1,S.empty) $ S.map (\t -> shift t (g2,t2)) t1
toGraph (Group []) = (HM.empty,S.singleton (0,0))

shift :: (Int,Int) -> (HM.HashMap (Int,Int) (S.Set (Int,Int)),S.Set (Int,Int)) -> (HM.HashMap (Int,Int) (S.Set (Int,Int)),S.Set (Int,Int))
shift (x,y) (grid,tips) = let f (p,q) = (p+x,q+y) in (HM.mapKeys f $ HM.map (S.map f) grid, S.map f tips)

move 'E' (x,y) = (x+1,y)
move 'S' (x,y) = (x,y-1)
move 'W' (x,y) = (x-1,y)
move 'N' (x,y) = (x,y+1)

getLongest :: Int -> S.Set (Int,Int) -> S.Set (Int,Int) -> HM.HashMap (Int,Int) (S.Set (Int,Int)) -> Int
getLongest steps finished frontier graph = let newFrontier = S.filter (\x -> x `S.notMember` finished && x `S.notMember` frontier) $ S.unions $ S.map fromJust $ S.filter isJust $ S.map (`HM.lookup` graph) frontier in if S.null newFrontier then steps else getLongest (steps+1) (S.union frontier finished) newFrontier graph

getLongest' :: Int -> S.Set (Int,Int) -> S.Set (Int,Int) -> HM.HashMap (Int,Int) (S.Set (Int,Int)) -> S.Set (Int,Int)
getLongest' steps finished frontier graph = let newFrontier = S.filter (\x -> x `S.notMember` finished && x `S.notMember` frontier) $ S.unions $ S.map fromJust $ S.filter isJust $ S.map (`HM.lookup` graph) frontier in if null frontier then S.empty else (if steps >= 1000 then S.union frontier else id) $ getLongest' (steps+1) (S.union frontier finished) newFrontier graph