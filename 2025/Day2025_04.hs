module Day2025_04 where
import Utility.AOC (enumerateFilterSet, neighbours8)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/04.txt"
    -- part 1
    let s = enumerateFilterSet (=='@') contents
    print $ S.size $ S.filter ((<4) . length . filter (`S.member` s) . neighbours8) s
    -- part 2
    print $ cull s

cull :: S.Set (Int,Int) -> Int
cull s = S.size a + if S.size a == 0 then 0 else cull b
    where
        a = S.filter ((<4) . length . filter (`S.member` s) . neighbours8) s
        b = S.filter ((>=4) . length . filter (`S.member` s) . neighbours8) s