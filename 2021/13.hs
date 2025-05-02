import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "13.txt"
    let (ps,fs) = parse contents
    -- part 1
    print $ S.size $ last fs ps
    -- part 2
    putStrLn $ pprint $ foldr ($) ps fs

parse :: String -> (S.Set (Int, Int), [S.Set (Int, Int) -> S.Set (Int, Int)])
parse contents =
    let [ps,fs] = splitOn "\n\n" contents
        points = S.fromList $ map ((\[a,b] -> (read a,read b)) . splitOn ",") $ lines ps
        helper l = case drop 11 l of
            'x':xs -> let c = read (tail xs) in S.map (\(x,y) -> (- abs (x-c) + c, y))
            'y':ys -> let c = read (tail ys) in S.map (\(x,y) -> (x, - abs (y-c) + c))
        folds = map helper $ reverse $ lines fs
    in (points,folds)

pprint :: S.Set (Int, Int) -> String
pprint points = unlines [concat [if (y,x) `S.member` points then "##"  else "  " | y <- [0..maximum (S.map fst points)]] | x <- [0..maximum (S.map snd points)]]