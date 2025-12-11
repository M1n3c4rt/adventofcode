module Day2025_10 where
import Data.List.Extra (splitOn, elemIndices)
import qualified Data.Set as S
import Data.LinearProgram hiding ((+), (-), sum)
import qualified Data.Map as M
import Data.Maybe (fromJust)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/10.txt"
    -- part 1
    print $ sum $ map ((\(a,b) -> solve b (S.singleton a)) . parse) $ lines contents
    -- part 2
    (print=<<) $ fmap sum $ mapM (uncurry solve' . parse') $ lines contents

parse :: String -> (S.Set Int, S.Set (S.Set Int))
parse str = let (s:ss) = words str in (S.fromList $ elemIndices '#' $ tail $ init s,S.fromList $ map commas (init ss))
    where commas = S.fromList . map read . splitOn "," . tail . init

parse' :: String -> ([S.Set Int],[Double])
parse' str = let (s:ss) = words str in (map (S.fromList . commas) (init ss),map fromIntegral $ commas (last ss))
    where commas = map read . splitOn "," . tail . init

apply :: Ord a => S.Set a -> S.Set a -> S.Set a
apply cur new = S.difference (S.union cur new) (S.intersection cur new)

solve :: S.Set (S.Set Int) -> S.Set (S.Set Int) -> Int
solve bs curs = if any S.null curs then 0 else 1 + solve bs (S.unions $ S.map (\c -> S.map (apply c) bs) curs)

solve' :: [S.Set Int] -> [Double] -> IO Int
solve' bs goal = round . fst . fromJust . snd <$> glpSolveVars (mipDefaults{msgLev=MsgOff}) problem
    where
        vecs = map (\n -> map (\b -> if S.member n b then 1 else 0) bs) [0..length goal-1]
        vars = map (('x':) . show) [0..length bs-1]
        problem = LP Min (lfunc 1) constrs (lfunc (LBound 0)) (lfunc IntVar)
        constrs = zipWith (\v g -> Constr Nothing (M.fromList $ zip vars v) (Equ g)) vecs goal
        lfunc b = M.fromList $ map (,b) vars