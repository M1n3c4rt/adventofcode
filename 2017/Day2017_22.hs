module Day2017_22 where
import qualified Data.Set as S
import Utility.AOC (enumerateFilterSet, enumerateFilter, traceSleepSeconds, prettyPrintHM, traceSleep)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/22.txt"
    -- part 1
    print $ length $ filter (\(a,b,c,d) -> a) $ take 10001 $ iterate burst (False,(12,-12),(0,1),S.map (\(x,y) -> (x,-y)) $ enumerateFilterSet (=='#') contents)
    -- part 2
    print $ length $ filter (\(a,b,c,d) -> a) $ take 10000001 $ iterate burst' (False,(12,-12),(0,1),HM.fromList $ map ((,I) . (\(x,y) -> (x,-y))) (enumerateFilter (=='#') contents))
    --mapM_ (putStrLn . prettyPrintHM . (\(a,b,c,d) -> HM.map toChar d) . traceSleep 10000) $ take 10000001 $ iterate burst' (False,(12,-12),(0,1),HM.fromList $ map ((,I) . (\(x,y) -> (x,-y))) (enumerateFilter (=='#') contents))

data NodeState = C | W | I | F deriving Show

toChar C = ' '
toChar W = 'W'
toChar I = '#'
toChar F = 'F'

burst :: (Bool,(Int,Int),(Int,Int),S.Set (Int,Int)) -> (Bool,(Int,Int),(Int,Int),S.Set (Int,Int))
burst (_,(x,y),(dx,dy),nodes) = next
    where
        next@(b,(x',y'),(dx',dy'),nodes') =
            if (x,y) `S.member` nodes then
                (False,(x+dx',y+dy'),(dy,-dx),S.delete (x,y) nodes)
            else
                (True,(x+dx',y+dy'),(-dy,dx),S.insert (x,y) nodes)

burst' :: (Bool,(Int,Int),(Int,Int),HM.HashMap (Int,Int) NodeState) -> (Bool,(Int,Int),(Int,Int),HM.HashMap (Int,Int) NodeState)
burst' (_,(x,y),(dx,dy),nodes) = next
    where
        next@(b,(x',y'),(dx',dy'),nodes') = case HM.lookupDefault C (x,y) nodes of
            C -> (False,(x+dx',y+dy'),(-dy, dx),HM.insert (x,y) W nodes)
            W -> (True, (x+dx',y+dy'),( dx, dy),HM.insert (x,y) I nodes)
            I -> (False,(x+dx',y+dy'),( dy,-dx),HM.insert (x,y) F nodes)
            F -> (False,(x+dx',y+dy'),(-dx,-dy),HM.insert (x,y) C nodes)