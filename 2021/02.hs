
main :: IO ()
main = do
    contents <- readFile "02.txt"
    -- part 1
    print $ uncurry (*) $ foldl move (0,0) $ lines contents
    -- part 2
    print $ (\(a,b,c) -> a*b) $ foldl moveAim (0,0,0) $ lines contents

move :: (Int, Int) -> String -> (Int, Int)
move (x,y) inst  =
    let [dir,lenC] = words inst
        len = read lenC
    in if dir == "forward" then (x+len,y) else if dir == "up" then (x,y-len) else (x,y+len)

moveAim :: (Int, Int, Int) -> String -> (Int, Int, Int)
moveAim (x,y,a) inst =
    let [dir,lenC] = words inst
        len = read lenC
    in
        if dir == "forward" then (x+len,y+len*a,a) else
            if dir == "up" then (x,y,a-len) else
                (x,y,a+len)