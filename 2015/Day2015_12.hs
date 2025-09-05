{-# LANGUAGE OverloadedStrings #-}
module Day2015_12 where

import Data.Aeson (decode, Value (..))
import Data.Aeson.KeyMap (toList, fromList)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Bifunctor (Bifunctor(second))

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/12.txt"
    let file = fromJust $ decode (pack contents) :: Value
    -- part 1
    print $ nums file
    -- part 2
    print $ nums $ removeRed file

removeRed (Object os) = Object $ fromList $ if any ((==String "red") . snd) $ toList os then [] else map (second removeRed) $ toList os
removeRed (Array as) = Array $ V.map removeRed as
removeRed x = x

nums (Number n) = round n
nums (Array as) = sum $ map nums $ V.toList as
nums (Object os) = sum $ map (nums . snd) $ toList os
nums _ = 0