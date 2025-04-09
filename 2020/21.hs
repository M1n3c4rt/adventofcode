import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, intercalate)

main :: IO ()
main = do
    contents <- readFile "21.txt"
    let ingredients = getIngredients contents
        food = HM.map (foldr1 S.intersection) $ foldr (\(k,c) acc -> HM.insertWith (++) k [c] acc) HM.empty $ concatMap parse (lines contents)
        resolved = resolve HM.empty food
    -- part 1
    print $ length $ filter (`notElem` HM.elems resolved) ingredients
    -- part 2
    putStrLn $ intercalate "," (map snd $ sortOn fst $ HM.toList resolved)

parse :: String -> [(String, S.Set String)]
parse line =
    let ws = words line
        (ingredients,rest) = span ((/='(') . head) ws
        allergens = splitOn "," $ init $ concat $ tail rest
    in map (,S.fromList ingredients) allergens

getIngredients :: String -> [String]
getIngredients = concatMap (words . takeWhile (/='(')) . lines

resolve :: HM.HashMap String String -> HM.HashMap String (S.Set String) -> HM.HashMap String String
resolve finished ls
    | HM.null ls = finished
    | otherwise =
        let singlets = HM.filter ((==1) . S.size) ls
            singletIngredients = S.unions $ HM.elems singlets
            updated = HM.map (S.filter (`S.notMember` singletIngredients)) $ HM.filter ((/=1) . S.size) ls
            newfinished = HM.union finished $ HM.map (S.elemAt 0) singlets
        in resolve newfinished updated