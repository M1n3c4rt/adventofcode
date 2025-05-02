import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Ratio ( Ratio, (%), numerator, denominator )

main :: IO ()
main = do
    contents <- readFile "21.txt"
    let monkeys = parse contents
    -- part 1
    print $ numerator $ fromJust $ HM.lookup "root" $ evaluate HM.empty [] monkeys
    -- part 2
    let f0 = evaluate' monkeys 0
    let f1 = evaluate' monkeys 1
    print $ numerator $ -(f0/(f1-f0))

type Monkeys = [(String,(String,String,Ratio Int -> Ratio Int -> Ratio Int))]
type FinishedMonkeys = HM.HashMap String (Ratio Int)

diffList :: Num a => [a] -> [a]
diffList (x:y:ys) = (y-x):diffList (y:ys)

func :: String -> (Ratio Int -> Ratio Int -> Ratio Int)
func x = case x of
    "+" -> (+)
    "*" -> (*)
    "-" -> (-)
    "/" -> (/)

parse :: String -> Monkeys
parse = map (helper . words) . lines
    where
        helper ls = case ls of
            [a,b] -> (take 4 a, ("none","none",const $ const $ read b%1))
            [a,b,c,d] -> (take 4 a, (b,d,func c))

evaluate :: FinishedMonkeys -> Monkeys -> Monkeys -> FinishedMonkeys
evaluate finished unfinished (m@(n,(a,b,f)):ms) = case (a,b) of
    ("none","none") -> evaluate (HM.insert n (f 0 0) finished) unfinished ms
    (p,q) -> case (HM.lookup p finished, HM.lookup q finished) of
        (Just x, Just y) -> evaluate (HM.insert n (f x y) finished) unfinished ms
        _ -> evaluate finished (m:unfinished) ms
evaluate finished [] [] = finished
evaluate finished unfinished [] = evaluate finished [] unfinished

evaluate' :: Monkeys -> Ratio Int -> Ratio Int
evaluate' ms n = fromJust $ HM.lookup "root" $ evaluate HM.empty [] $ map helper ms
    where helper k@(s,(a,b,f))
            | s == "root" = (s,(a,b,(-)))
            | s == "humn" = (s,(a,b,const $ const n))
            | otherwise = k