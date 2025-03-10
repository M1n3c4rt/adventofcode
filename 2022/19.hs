import Data.Char (isDigit)
import Data.List (nub, sortOn)

main :: IO ()
main = do
    contents <- readFile "19.txt"
    -- part 1
    print $ sum $ zipWith (*) [1..] $ map (\bp@(b,c,d,e) -> maximum $ map (\t -> maxGeodes t 24 bp (0,0,0,0) (1,0,0,0)) "pq") $ parse contents
    -- part 2
    print $ product $ map (\bp -> maximum $ map (\t -> maxGeodes t 32 bp (0,0,0,0) (1,0,0,0)) "pq") $ take 3 $ parse contents

type Blueprint = (Int, Int, (Int, Int), (Int, Int))
type Robots = (Int,Int,Int,Int)
type Resources = (Int,Int,Int,Int)

parse :: String -> [Blueprint]
parse = map ((\[a,b,c,d,e,f,g] -> (b,c,(d,e),(f,g))) . map (read . takeWhile isDigit) . filter (isDigit . head) . words) . lines

add4 :: Robots -> Robots -> Robots
add4 (a,b,c,d) (e,f,g,h) = (a+e,b+f,c+g,d+h)

mult4 :: Int -> Robots -> Robots
mult4 n (a,b,c,d) = (n*a,n*b,n*c,n*d)

maxGeodes :: Char -> Int -> Blueprint -> Resources -> Robots -> Int
maxGeodes _ 0 _ (_,_,_,s) _ = s
maxGeodes target n bp@(b,c,(d,e),(f,g)) rss@(p,q,r,s) rbs@(w,x,y,z) = case target of
    'p' -> if p < b then maxGeodes target (n-1) bp (add4 rss rbs) rbs else
        maximum $ map (\t -> maxGeodes t (n-1) bp (p+w-b,q+x,r+y,s+z) (w+1,x,y,z)) nexts
    'q' -> if p < c then maxGeodes target (n-1) bp (add4 rss rbs) rbs else
        maximum $ map (\t -> maxGeodes t (n-1) bp (p+w-c,q+x,r+y,s+z) (w,x+1,y,z)) nexts
    'r' -> if p < d || q < e then maxGeodes target (n-1) bp (add4 rss rbs) rbs else
        maximum $ map (\t -> maxGeodes t (n-1) bp (p+w-d,q+x-e,r+y,s+z) (w,x,y+1,z)) nexts
    's' -> if p < f || r < g then maxGeodes target (n-1) bp (add4 rss rbs) rbs else
        maximum $ map (\t -> maxGeodes t (n-1) bp (p+w-f,q+x,r+y-g,s+z) (w,x,y,z+1)) nexts
    where nexts = concat
            [
                ['p' | w < maximum [b,c,d,f]],
                ['q' | x < e],
                ['r' | y < g, x > 0],
                ['s' | y > 0]
            ]