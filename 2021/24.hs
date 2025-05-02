import Data.List (genericDrop)

main :: IO ()
main = do
    contents <- readFile "24.txt"
    -- used these variable instructions to determine digit constraints, and then solved the problem manually lmao
    -- also wrote a function to verify the answers, of course
    let (ps,qs,vs) = (parseps contents, parseqs contents, parsevs contents)
    -- part 1
    print $ evaluate (lines contents) [5,9,6,9, 2,9,9,4, 9,9,4,9, 9,8] (0,0,0,0)
    -- part 2
    print $ evaluate (lines contents) [1,6,1,8, 1,1,1,1, 6,4,1,5, 2,1] (0,0,0,0)

dropEvery :: Integer -> [a] -> [a]
dropEvery n (x:xs) = x:dropEvery n (genericDrop (n-1) xs)
dropEvery n [] = []

parseps :: String -> [Integer]
parseps = reverse . map (read . drop 6) . dropEvery 2 . tail . filter (\l -> take 5 l == "add x") . lines

parseqs :: String -> [Integer]
parseqs = reverse . map (read . drop 6) . dropEvery 4 . drop 3 . filter (\l -> take 5 l == "add y") . lines

parsevs :: String -> [Bool]
parsevs = reverse . map ((==26) . read . drop 6) . filter (\l -> take 5 l == "div z") . lines


validate :: [Integer] -> [Integer] -> [Integer] -> [Bool] -> Integer
validate [] _ _ _ = 0
validate (d:digits) (p:ps) (q:qs) (v:vs) = prev*(25*n + 1) + n*(d+q)
    where
        prev = (if v then (`div`26) else id) $ validate digits ps qs vs
        n = fromIntegral $ fromEnum $ (prev `mod` 26) + p /= d

solveFor :: Int -> [Int] -> [Int] -> [Bool] -> [[Int]]
solveFor _ [] [] [] = []
solveFor k (p:ps) (q:qs) (v:vs) = prevs
    where
        prevs = concat [map (d:) (solveFor pr ps qs vs) | d <- [1 .. 9],
            let pr1 = map fst $ filter ((==k) . snd) $ takeWhile ((<=k) . snd) $ map (\r -> (r,f r)) $ filter (`n` d) [0..],
            let pr2 = map fst $ filter ((==k) . snd) $ takeWhile ((<=k) . snd) $ map (\r -> (r,f r*26+r+d)) $ filter (not . (`n` d)) [0..],
            pr <- pr1++pr2]
        n pr d = (pr `mod` 26) + p /= d
        f = if v then (`div`26) else id

evaluate :: [String] -> [Integer] -> (Integer, Integer, Integer, Integer) -> Integer
evaluate (inst:insts) input state@(x,y,z,w) = case words inst of
    ["inp",a] -> evaluate insts (tail input) $ h a (const $ head input)
    ["add",a,b] -> evaluate insts input $ h a (+r b)
    ["mul",a,b] -> evaluate insts input $ h a (*r b)
    ["div",a,b] -> evaluate insts input $ h a (`div` r b)
    ["mod",a,b] -> evaluate insts input $ h a (`mod` r b)
    ["eql",a,b] -> evaluate insts input $ h a (fromIntegral . fromEnum . (==r b))
    where
        h a f = case a of
            "x" -> (f x,y,z,w)
            "y" -> (x,f y,z,w)
            "z" -> (x,y,f z,w)
            "w" -> (x,y,z,f w)
        r b = case b of
            "x" -> x
            "y" -> y
            "z" -> z
            "w" -> w
            c -> read c
evaluate [] _ (x,y,z,w) = z

base26 :: Integral a => a -> [a]
base26 0 = []
base26 x = let (q,r) = x `divMod` 26 in base26 q ++ [r]