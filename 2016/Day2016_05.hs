{-# LANGUAGE BangPatterns #-}
module Day2016_05 where

import Data.Hash.MD5 ( md5s, Str(Str) )
import Data.List (sortOn)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/05.txt"
    -- part 1
    putStrLn $ map (!!5) $ take 8 $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    -- part 2 (featuring "cinematic" decrypting animation)
    putStr "DECRYPTING [        ]\r"
    let !p20 = (!!6) $ head $ filter ((=='0') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [#       ]\r"
    let !p21 = (!!6) $ head $ filter ((=='1') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [##      ]\r"
    let !p22 = (!!6) $ head $ filter ((=='2') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [###     ]\r"
    let !p23 = (!!6) $ head $ filter ((=='3') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [####    ]\r"
    let !p24 = (!!6) $ head $ filter ((=='4') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [#####   ]\r"
    let !p25 = (!!6) $ head $ filter ((=='5') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [######  ]\r"
    let !p26 = (!!6) $ head $ filter ((=='6') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [####### ]\r"
    let !p27 = (!!6) $ head $ filter ((=='7') . (!!5)) $ filter ((=="00000") . take 5) $ map (md5s . Str . (contents++) . show) [0..]
    putStr "DECRYPTING [########]\r"
    threadDelay 500000
    putStrLn "DECRYPTION COMPLETE. ACCESS GRANTED."
    putStrLn $ "PASSWORD: " ++ [p20,p21,p22,p23,p24,p25,p26,p27]
