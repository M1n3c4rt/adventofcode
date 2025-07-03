module Day2017_08 where
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/08.txt"
    -- part 1
    print $ maximum $ foldl runInst HM.empty $ map words $ lines contents
    -- part 2
    print $ maximum $ foldr (HM.unionWith max) HM.empty $ scanl runInst HM.empty $ map words $ lines contents

runInst :: HM.HashMap String Int -> [String] -> HM.HashMap String Int
runInst regs [reg,dir,n,_,creg,cond,c] =
    if HM.lookupDefault 0 creg regs `condition` read c then
        HM.insert reg (HM.lookupDefault 0 reg regs `inst` read n) regs
    else regs
    where
        condition = case cond of
            ">" -> (>)
            "<" -> (<)
            ">=" -> (>=)
            "<=" -> (<=)
            "==" -> (==)
            "!=" -> (/=)
        inst = case dir of
            "inc" -> (+)
            "dec" -> (-)