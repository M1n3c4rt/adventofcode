main :: IO ()
main = do
    contents <- readFile "6.txt"
    -- part 1
    print $ product $ map solveQuadratic $ parseInput contents
    -- part 2
    print $ solveQuadratic $ parseInput' contents

parseInput :: String -> [(Float,Float)]
parseInput = (\[a,b] -> zip a b) . map (map read . words . tail . dropWhile (/=':')) . lines

parseInput' :: String -> (Double,Double)
parseInput' = (\[a,b] -> (a,b)) . map (read . concat . words . tail . dropWhile (/=':')) . lines

solveQuadratic :: (RealFrac a1, Floating a1) => (a1, a1) -> Int
solveQuadratic (t,d) = 1 + floor ((t + sqrt (t^2 - 4*d))/2) - ceiling ((t - sqrt (t^2 - 4*d))/2)