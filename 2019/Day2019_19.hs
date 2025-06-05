module Day2019_19 where
import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Intcode ( CompilerState, parse, runC, getOutput )

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2019/19.txt"
    -- part 1
    print $ HM.size $ HM.filter id $ grid 49 $ parse contents
    -- part 2
    print $ initFindSquare $ parse contents
    --print $ map (\(x,y) -> (x,-y)) $ HM.keys $ HM.filter id $ grid 151 $ parse contents

grid :: Int -> CompilerState -> HM.HashMap (Int, Int) Bool
grid n state = HM.fromList [((x,y),getOutput (runC [x,y] state)==[1]) | x <- [0..n], y <- [0..n]]

pprint :: Int -> HM.HashMap (Int, Int) Bool -> String
pprint n g = unlines [[if fromJust (HM.lookup (x,y) g) then '#' else '.' | x <- [0..n]] | y <- [0..n]]

findSquare :: (Int, Int) -> (Int, Int) -> CompilerState -> Int
findSquare (a,b) (c,d) state
    | c - a == 99 && b - d == 99 = 10000 * a + d
    | otherwise =
        let (a',b') = (a,b+1)
            (c',d') = (c+1,d)

            southWest = map (\(x,y) -> ((x,y),getOutput (runC [x,y] state))) . iterate (\(x,y) -> (x-1,y+1))
            northEast = map (\(x,y) -> ((x,y),getOutput (runC [x,y] state))) . iterate (\(x,y) -> (x+1,y-1))

            (newa,newb) = case getOutput $ runC [a',b'] state of
                [1] -> fst $ last $ takeWhile ((==[1]) . snd) $ southWest (a',b')
                [0] -> fst $ head $ dropWhile ((==[0]) . snd) $ northEast (a',b')
            (newc,newd) = case getOutput $ runC [c',d'] state of
                [1] -> fst $ last $ takeWhile ((==[1]) . snd) $ northEast (c',d')
                [0] -> fst $ head $ dropWhile ((==[0]) . snd) $ southWest (c',d')
        in findSquare (newa,newb) (newc,newd) state

initFindSquare :: CompilerState -> Int
initFindSquare state =
    let (a,b) = head $ dropWhile (\(x,y) -> getOutput (runC [x,y] state) == [0]) $ iterate (\(x,y) -> (x-1,y+1)) (75,0)
        (c,d) = head $ dropWhile (\(x,y) -> getOutput (runC [x,y] state) == [0]) $ iterate (\(x,y) -> (x+1,y-1)) (0,75)
    in findSquare (a,b) (c,d) state