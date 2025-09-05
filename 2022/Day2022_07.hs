module Day2022_07 where

import qualified Data.HashMap.Strict as HM
import Data.Char (isDigit)
import Data.Either (rights)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/07.txt"
    let newTree = insert [] emptyTree $ lines contents
    let dirSizes = rights $ getDirSizes newTree
    -- print the whole tree
    putStrLn $ pprint "/" 0 $ insert [] emptyTree $ lines contents
    -- part 1
    print $ sum $ filter (<100000) dirSizes
    -- part 2
    print $ minimum $ filter (>= head dirSizes - 40000000) dirSizes

data Tree = Dir (HM.HashMap String Tree) | File Int deriving Show

emptyTree :: Tree
emptyTree = Dir HM.empty

insert :: [String] -> Tree -> [String] -> Tree
insert current tree (l:ls)
    | take 4 l == "$ cd" = case drop 5 l of
        ".." -> insert (tail current) tree ls
        "/" -> insert [] tree ls
        s -> insert (s:current) tree ls
    | isDigit (head l) = let [size,name] = words l in
        insert current (insFile (reverse current) name (read size) tree) ls
    | otherwise = insert current tree ls
    where
          insFile :: [String] -> String -> Int -> Tree -> Tree
          insFile [] name size (Dir k) = Dir $ HM.insert name (File size) k
          insFile (d:cur) name size (Dir m) = let newm = HM.insertWith (\a (Dir b) -> if HM.null b then a else Dir b) d (Dir HM.empty) m in
            Dir $ HM.adjust (insFile cur name size) d newm
insert current tree [] = tree

pprint :: String -> Int -> Tree -> String
pprint name indent tree = case tree of
    Dir m -> init $ replicate indent ' ' ++ "- " ++ name ++ " (dir)\n" ++ unlines (map (\(n,d) -> pprint n (indent+1) d) $ HM.toList m)
    File s -> replicate indent ' ' ++ "- " ++ name ++ " (file, size=" ++ show s ++ ")"

getDirSizes :: Tree -> [Either Int Int]
getDirSizes tree = case tree of
    File s -> [Left s]
    Dir m -> let children = map getDirSizes $ HM.elems m in
        Right (sum $ map (helper . head) children) : concat children
    where helper (Right n) = n
          helper (Left n) = n