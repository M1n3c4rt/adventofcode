import Data.List.Split (splitOn)
import Data.List (groupBy)
import Data.Char (isDigit, isAlphaNum)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isNothing)

main :: IO ()
main = do
    contents <- readFile "19.txt"
    let (workflows,parts) = parse contents
    let start = HM.lookupDefault [] "in" workflows
    -- part 1
    print $ sum $ map sum $ filter (\p -> checkWorkflows p start workflows) parts
    -- part 2
    print $ checkWorkflows' [(1,4000),(1,4000),(1,4000),(1,4000)] start workflows

type Part = [Int]
type PartRange = [(Int,Int)]
type Workflow = [((Int,Int,Ordering),String)]

parse :: String -> (HM.HashMap String Workflow, [Part])
parse contents = let [a,b] = splitOn "\n\n" contents in (HM.fromList $ map parseWorkflows $ lines a, map parseParts $ lines b)

parseParts :: String -> Part
parseParts = map read . filter (any isDigit) . groupBy ((==) `on` isDigit)

parseWorkflows :: String -> (String, Workflow)
parseWorkflows s = let (name,rest) = span (/='{') s in
    let fs = splitOn "," $ init $ tail rest in
        (name,map parseFunc fs)

parseFunc :: String -> ((Int,Int,Ordering), String)
parseFunc s
    | ':' `notElem` s = ((0,0,GT), s)
    | otherwise = let [prop,cond,num,_,workflow] = groupBy ((==) `on` isAlphaNum) s in
        let f = if cond == "<" then LT else GT in
            let g | prop == "x" = 0
                  | prop == "m" = 1
                  | prop == "a" = 2
                  | prop == "s" = 3 in
            ((g,read num,f),workflow)

truncateRange :: (Int,Int,Ordering) -> PartRange -> (Maybe PartRange,Maybe PartRange)
truncateRange (i,n,o) partrange = 
    let (a,b) = partrange !! i
        genRange f = take i partrange ++ [f (a, b)] ++ drop (i+1) partrange;
        validateRange xs = if any (\(a,b) -> b-a < 0) xs then Nothing else Just xs;
        [p, q] = map (validateRange . genRange) [helper, helper']
        in (p, q)
    where helper (r,s)
            | o == GT && r < n && n < s = (n+1,s)
            | o == LT && r < n && n < s = (r,n-1)
            | (o == GT && n < r) || (o == LT && n > s) = (r,s)
            | (o == GT && n > s) || (o == LT && n < r) = (-1,-2)
          helper' (r,s)
            | o == GT && r < n && n < s = (r,n)
            | o == LT && r < n && n < s = (n,s)
            | (o == GT && n < r) || (o == LT && n > s) = (-1,-2)
            | (o == GT && n > s) || (o == LT && n < r) = (r,s)

checkWorkflows :: Part -> Workflow -> HM.HashMap String Workflow -> Bool
checkWorkflows part (((i,n,o),w):workflow) workflows
    | part !! i `compare` n == o = (w /= "R") && ((w == "A") || checkWorkflows part (HM.lookupDefault [] w workflows) workflows)
    | otherwise = checkWorkflows part workflow workflows

checkWorkflows' :: PartRange -> Workflow -> HM.HashMap String Workflow -> Int
checkWorkflows' partrange ((f,w):workflow) workflows = let (ins,outs) = truncateRange f partrange in
    let as
          | isNothing ins = 0
          | w == "R" = 0
          | w == "A" = let Just a = ins in size a
          | otherwise = let Just a = ins in checkWorkflows' a (HM.lookupDefault [] w workflows) workflows in
            let bs = if isNothing outs then 0 else let Just b = outs in checkWorkflows' b workflow workflows in
                as + bs
checkWorkflows' partrange [] workflows = 0

size :: PartRange -> Int
size = product . map (\(a,b) -> 1+b-a)