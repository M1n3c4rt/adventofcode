module Day2016_14 where
import Data.List (group, find)
import Data.Maybe (isJust, mapMaybe)
import Control.Arrow (Arrow(second))
import Crypto.Hash.MD5 ( hash )
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/14.txt"
    -- part 1
    let hashes = map (\n -> (n,) . getHash . (contents++) . show $ n) [0..]
        hashes' = map (\n -> (n,) . (!!2017) . iterate getHash . (contents++) . show $ n) [0..]
        threes = mapMaybe (\(a,b) -> (a,) <$> run 3 b)
        fives = mapMaybe (\(a,b) -> (a,) <$> run 5 b)
    print $ fst $ (!!63) $ keys (threes hashes) (fives hashes)
    -- part 2
    print $ fst $ (!!63) $ keys (threes hashes') (fives hashes')

run :: Int -> String -> Maybe Char
run n h = head <$> find ((>=n) . length) (group h)

keys :: [(Int,Char)] -> [(Int,Char)] -> [(Int,Char)]
keys ((n,t):threes) fives
    | n'-n <= 1000 = (n,t):keys threes newFives
    | otherwise = keys threes newFives
    where
        newFives = dropWhile ((<=n) . fst) fives
        Just (n',t') = find ((==t) . snd) newFives

getHash :: String -> String
getHash bs = unpack $ encode $ hash $ pack bs