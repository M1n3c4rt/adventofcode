import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List (findIndex, elemIndex, intercalate, nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    handle <- openFile "15.txt" ReadMode
    contents <- hGetContents handle
    -- part 1
    let [grid,stream] = splitOn "\n\n" contents in putStr $ show $ getGPS 0 $ moveAll (lines grid) (filter (/= '\n') stream)
    putStr "\n"
    -- part 2
    let [grid,stream] = splitOn "\n\n" contents in putStr $ show $ getGPSM 0 $ moveAll' (map expandGrid $ lines grid) (filter (/= '\n') stream)
    putStr "\n"
    hClose handle

expandGrid :: [Char] -> [Char]
expandGrid (s:ss)
    | s == '.' = ".."++expandGrid ss
    | s == 'O' = "[]"++expandGrid ss
    | s == '@' = "@."++expandGrid ss
    | s == '#' = "##"++expandGrid ss
expandGrid [] = []

getGuard :: [[Char]] -> (Int, Int)
getGuard l = let i = fromMaybe (-1) $ findIndex (\x -> '@' `elem` x) l in (fromMaybe (-1) $ elemIndex '@' (l !! i),i)

move :: Int -> [Char] -> [Char]
move n s
    | moveable n s = move' n s
    | otherwise = s

move' :: Int -> [Char] -> [Char]
move' 0 (r:s:ss) = '.':move' (-1) (r:s:ss)
move' (-1) (r:s:ss)
    | s == '.' = r:ss
    | otherwise = r:move' (-1) (s:ss)
move' n (s:ss) = s:move' (n-1) ss

moveable :: Int -> [Char] -> Bool
moveable 0 (s:ss)
    | s == '.' = True
    | s == '#' = False
    | otherwise = moveable 0 ss
moveable n (s:ss) = moveable (n-1) ss

moveableM :: (Int, Int) -> [[Char]] -> Bool
moveableM (x,0) (rr:ss:sss)
    | ss !! x == ']' = moveableM (x,0) (ss:sss) && moveableM (x-1,0) (ss:sss)
    | ss !! x == '[' = moveableM (x,0) (ss:sss) && moveableM (x+1,0) (ss:sss)
    | ss !! x == '.' = True
    | ss !! x == '#' = False
moveableM (x,y) (ss:sss) = moveableM (x,y-1) sss

moveRight :: (Int, Int) -> [[Char]] -> [[Char]]
moveRight (x,y) l = take y l ++ [move x (l !! y)] ++ drop (y+1) l

moveLeft :: (Int, Int) -> [[Char]] -> [[Char]]
moveLeft (x,y) l = take y l ++ [reverse $ move (length (head l) - 1 - x) $ reverse (l !! y)] ++ drop (y+1) l

moveDown :: (Int, Int) -> [[Char]] -> [[Char]]
moveDown (x,y) l = zipWith3 (\ x y z -> x ++ [y] ++ z) (map (take x) l) (move y $ map (!!x) l) (map (drop (x+1)) l)

moveUp :: (Int, Int) -> [[Char]] -> [[Char]]
moveUp (x,y) l = zipWith3 (\ x y z -> x ++ [y] ++ z) (map (take x) l) (reverse $ move (length l - 1 - y) $ reverse $ map (!!x) l) (map (drop (x+1)) l)

findDownM :: Int -> (Int, Int) -> [[Char]] -> [(Int, Int)]
findDownM n (x,0) (rr:ss:sss)
    | ss !! x == '[' = (x,n):(findDownM (n+1) (x,0) (ss:sss) ++ findDownM (n+1) (x+1,0) (ss:sss))
    | ss !! x == ']' = (x,n):(findDownM (n+1) (x,0) (ss:sss) ++ findDownM (n+1) (x-1,0) (ss:sss))
    | otherwise = [(x,n)]
findDownM n (x,y) (ss:sss) = findDownM (n+1) (x,y-1) sss

repeatMoveDown :: [(Int, Int)] -> [[Char]] -> [[Char]]
repeatMoveDown ((x,y):xys) l = repeatMoveDown xys (moveDown (x,y) l)
repeatMoveDown [] l = l

moveDownM :: (Int, Int) -> [[Char]] -> [[Char]]
moveDownM (x,y) l
    | moveableM (x,y) l = repeatMoveDown (nubTop $ findDownM 0 (x,y) l) l
    | otherwise = l

moveUpM :: (Int, Int) -> [[Char]] -> [[Char]]
moveUpM (x,y) l = reverse $ moveDownM (x,length l - 1 - y) $ reverse l

nubTop :: [(Int, Int)] -> [(Int, Int)]
nubTop l = sortOn ((0-) . snd) $ nub l

moveAll :: [[Char]] -> [Char] -> [[Char]]
moveAll grid (s:stream)
    | s == '>' = moveAll (moveRight (getGuard grid) grid) stream
    | s == '<' = moveAll (moveLeft (getGuard grid) grid) stream
    | s == 'v' = moveAll (moveDown (getGuard grid) grid) stream
    | s == '^' = moveAll (moveUp (getGuard grid) grid) stream
    | otherwise = grid
moveAll grid [] = grid

moveAll' :: [[Char]] -> [Char] -> [[Char]]
moveAll' grid (s:stream)
    | s == '>' = moveAll' (moveRight (getGuard grid) grid) stream
    | s == '<' = moveAll' (moveLeft (getGuard grid) grid) stream
    | s == 'v' = moveAll' (moveDownM (getGuard grid) grid) stream
    | s == '^' = moveAll' (moveUpM (getGuard grid) grid) stream
    | otherwise = grid
moveAll' grid [] = grid

getGPS :: Int -> [[Char]] -> Int
getGPS n (ss:sss) = getGPS' n 0 ss + getGPS (n+1) sss
getGPS _ [] = 0

getGPS' :: Int -> Int -> [Char] -> Int
getGPS' n m (s:ss)
    | s == 'O' = 100*n + m + getGPS' n (m+1) ss
    | otherwise = getGPS' n (m+1) ss
getGPS' _ _ [] = 0

getGPSM :: Int -> [[Char]] -> Int
getGPSM n (ss:sss) = getGPSM' n 0 ss + getGPSM (n+1) sss
getGPSM _ [] = 0

getGPSM' :: Int -> Int -> [Char] -> Int
getGPSM' n m (s:ss)
    | s == '[' = 100*n + m + getGPSM' n (m+1) ss
    | otherwise = getGPSM' n (m+1) ss
getGPSM' _ _ [] = 0