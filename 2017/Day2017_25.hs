module Day2017_25 where
import Data.List.Extra (splitOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/25.txt"
    let (state,step,states) = parse contents
    -- part 1
    print $ S.size $ (\(a,b,c) -> c) $ (!!step) $ iterate (run states) (0,state,S.empty)
    -- part 2

parse :: String -> (Char, Int, HM.HashMap Char ((Bool, Bool, Char), (Bool, Bool, Char)))
parse s = let x:xs = splitOn "\n\n" s; [a,b] = lines x in (head $ last $ words a, read $ last $ init $ words b, HM.fromList $ map parseChunk xs) 

parseChunk :: String -> (Char, ((Bool, Bool, Char), (Bool, Bool, Char)))
parseChunk s = let [a,_,c,d,e,_,g,h,i] = lines s in
    (head $ last $ words a,
        (
            ((=="1.") $ last $ words c,(=="right.") $ last $ words d,head $ last $ words e),
            ((=="1.") $ last $ words g,(=="right.") $ last $ words h,head $ last $ words i)
        )
    )

run :: HM.HashMap Char ((Bool, Bool, Char), (Bool, Bool, Char)) -> (Int, Char, S.Set Int) -> (Int, Char, S.Set Int)
run states (pos,state,tape) = (pos + if m then 1 else -1,s,(if o then S.insert else S.delete) pos tape)
    where
        Just (i1,i2) = HM.lookup state states
        (o,m,s) = if S.member pos tape then i2 else i1