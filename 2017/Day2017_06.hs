module Day2017_06 where
import Data.Foldable (maximumBy)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/06.txt"
    let ns = HM.fromList $ zip [0..] $ map read $ words contents
    -- part 1
    print $ period S.empty 0 $ iterate (redist $ length ns) ns
    -- part 2
    print $ period' HM.empty 0 $ iterate (redist $ length ns) ns

redist :: Int -> HM.HashMap Int Int -> HM.HashMap Int Int
redist size ns = helper ((a+1)`mod`size) b $ HM.insert a 0 ns
    where
        (a,b) = maximumBy (compare `on` \(x,y) -> (y,-x)) $ HM.toList ns
        helper x 0 ns' = ns'
        helper x y ns' = helper ((x+1) `mod` size) (y-1) $ HM.adjust (+1) x ns'

period finished steps (l:ls)
    | l `S.member` finished = steps
    | otherwise = period (S.insert l finished) (steps+1) ls

period' finished steps (l:ls) = case HM.lookup l finished of
    Nothing -> period' (HM.insert l steps finished) (steps+1) ls
    Just n -> steps-n