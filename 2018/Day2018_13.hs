{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Day2018_13 where
import Utility.AOC (enumerateHM)
import qualified Data.HashMap.Strict as HM
import Data.Complex ( Complex(..) )
import Data.Function (on)
import Data.List (groupBy, sortOn, nubBy)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/13.txt"
    let grid = HM.mapKeys (\(x,y) -> x :+ (-y)) $ enumerateHM contents
    -- part 1
    putStrLn $ (\(x:+y) -> init $ tail $ show (x,-y)) $ moveMinecarts (toTracks grid) (toMinecarts grid)
    -- part 2
    putStrLn $ (\(x:+y) -> init $ tail $ show (x,-y)) $ pos $ moveMinecarts' (toTracks grid) (toMinecarts grid)

data Minecart = Minecart {pos :: Complex Int, dir :: Complex Int, trinity :: Complex Int}
type Track = Minecart -> Minecart

up :: Complex Int
down :: Complex Int
left :: Complex Int
right :: Complex Int
tleft :: Complex Int
tright :: Complex Int
tstraight :: Complex Int
(up,down,left,right,tleft,tright,tstraight) = (0:+1,0:+(-1),(-1):+0,1:+0,up,down,right)

toMinecarts :: HM.HashMap (Complex Int) Char -> [Minecart]
toMinecarts = HM.elems . HM.mapWithKey (\k v -> Minecart k (helper v) tleft) . HM.filter (`elem` "<>^v")
    where helper c =
            case c of
                '>' -> right
                '<' -> left
                '^' -> up
                'v' -> down

toTracks :: HM.HashMap (Complex Int) Char -> HM.HashMap (Complex Int) Track
toTracks grid = HM.mapWithKey (toTrack grid) grid

toTrack :: HM.HashMap (Complex Int) Char -> Complex Int -> Char -> Track
toTrack grid p c = case c of
    '|' -> (\m -> m {pos=pos m + dir m})
    '-' -> (\m -> m {pos=pos m + dir m})
    '/' -> (\m -> m {pos=pos m + invert (dir m), dir=invert (dir m)})
    '\\' -> (\m -> m {pos=pos m + invert' (dir m), dir=invert' (dir m)})
    '+' -> (\m -> let t = trinity m in if t == tleft then
            m {pos=pos m + dir m*tleft, dir=dir m*tleft, trinity=tstraight} else if t == tstraight then
            m {pos=pos m + dir m, trinity=tright} else
            m {pos=pos m + dir m*tright, dir=dir m*tright, trinity=tleft}
            )
    ' ' -> id
    '<' -> toTrack grid p '-'
    '>' -> toTrack grid p '-'
    '^' -> toTrack grid p '|'
    'v' -> toTrack grid p '|'
    where
        invert (x:+y) = y:+x
        invert' (p::Complex Int) = negate $ invert p


moveMinecarts :: HM.HashMap (Complex Int) Track -> [Minecart] -> Complex Int
moveMinecarts tracks ms
    | null candidates = moveMinecarts tracks $ map helper $ sortOn pos ms
    | otherwise = head candidates
    where
        collisions = map (pos . head) . filter ((>1) . length) . groupBy ((==) `on` pos)
        helper m = HM.lookupDefault id (pos m) tracks m
        scanmap (x:xs) = (helper x:xs):map (helper x:) (scanmap xs)
        scanmap [] = []
        scanned = scanmap $ sortOn pos ms
        candidates = concatMap collisions scanned

moveMinecarts' :: HM.HashMap (Complex Int) Track -> [Minecart] -> Minecart
moveMinecarts' tracks ms
    | length ms == 1 = head ms
    | otherwise = moveMinecarts' tracks $ safe $ foldCollisions $ sortOn pos ms
    where
        safe = concat . filter ((==1) . length) . groupBy ((==) `on` pos)
        helper m = HM.lookupDefault id (pos m) tracks m

        foldCollisions (r:rest) = let moved = helper r in
            if any ((==pos moved) . pos) rest then
                foldCollisions (filter ((/=pos moved) . pos) rest)
            else moved : foldCollisions rest
        foldCollisions [] = []

instance {-# INCOHERENT #-} Num (Complex Int) where
    (a:+b)+(c:+d) = (a+c):+(b+d)
    (a:+b)*(c:+d) = (a*c-b*d):+(b*c+a*d)
    abs (a:+b) = fromIntegral $ abs a + abs b
    signum (a:+b) = signum a :+ signum b
    negate (a:+b) = negate a :+ negate b
    fromInteger n = fromInteger n :+ 0

instance Ord (Complex Int) where
    compare :: Complex Int -> Complex Int -> Ordering
    compare (a :+ b) (c :+ d)
        | b == d = compare a c
        | otherwise = compare (-b) (-d)

instance Show Track where
    show :: Track -> String
    show t = show $ t (Minecart 0 1 1)

instance Show Minecart where
    show :: Minecart -> String
    show (Minecart (x:+y) d t) = show (x,y) ++ " " ++ dir ++ " " ++ trinity
        where
            dir
                | d == up = "up"
                | d == down = "down"
                | d == left = "left"
                | otherwise = "right"
            trinity
                | t == tleft = "left"
                | t == tstraight = "straight"
                | t == tright = "right"