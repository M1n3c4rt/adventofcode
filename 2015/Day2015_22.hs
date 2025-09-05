module Day2015_22 where
import Utility.AOC (numbers)
import System.Random (Random (randomR), newStdGen)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/22.txt"
    -- literally the worst day ever i hate it so much
    -- part 1
    let [h,d] = numbers contents
    print $ (!!100) $ scanl1 min $ map fst $ floodFillGoal False True [(0,startingState h d)]
    -- part 2
    print $ (!!100) $ scanl1 min $ map fst $ floodFillGoal True True [(0,startingState h d)]

data State = State {effects :: [Effect], hp :: Int, mana :: Int, bosshp :: Int, bossdmg :: Int}
type Effect = (Bool,Int,Int,State -> State)

interactive :: Bool -> Bool -> (Int, State) -> IO ()
interactive hardMode parity state = do
    print state
    let states = updateState hardMode parity state
    print states
    n <- read <$> getLine
    interactive hardMode (not parity) (states !! n)

random :: Bool -> Bool -> (Int,State) -> (Int, State) -> IO ()
random hardMode parity origState state
    | (\(n,s) -> n == 1269 && bosshp s <= 0) state = do print state; print $ Just $ fst state
    | (\(n,s) -> n > 1269 || hp s <= 0) state = random hardMode parity origState origState
    | otherwise = do
        print state
        let states = updateState hardMode parity state
        n <- newStdGen
        random hardMode (not parity) origState (states !! fst (randomR (0,length states-1) n))

startingState = State [] 50 500

spells :: [Effect]
spells = [
        (True,53,1,\state -> state {bosshp = bosshp state - 4}),
        (True,73,1,\state -> state {bosshp = bosshp state - 2, hp = hp state + 2}),
        (False,113,6,\state -> state {hp = if bossdmg state <= 7 then hp state + bossdmg state - 1 else hp state + 7}),
        (False,173,6,\state -> state {bosshp = bosshp state - 3}),
        (False,229,5,\state -> state {mana = mana state + 101})
    ]

updateState :: Bool -> Bool -> (Int,State) -> [(Int, State)]
updateState hardMode playerTurn (n',state) = applyEffects
    where
        initState = if playerTurn then state {hp = hp state - if hardMode then 1 else 0} else state {hp = hp state - bossdmg state}
        expired = initState {bosshp = if hp initState == 0 then 500 else bosshp initState {-this is a terrible hack i am so sorry-}, effects = filter (\(_,_,d,_) -> d > 0) $ effects initState}
        candidates = if playerTurn then filter (\(b,m,d,f) -> m `notElem` map getMana (filter (\(_,_,d',_) -> d' > 1) $ effects initState) && m <= mana expired) spells else []
        insertCandidates = map (\c -> (getMana c,expired {mana = mana expired - getMana c,effects = c:effects expired})) candidates
        applyEffects = map (\(n,s) -> (n'+n,) $ (\s' -> s' {effects = map (\(b,m,d,f) -> (True,m,if b then d-1 else d,f)) (effects s')}) $ foldr (\(b,m,d,f) acc -> if b && not (m == 113 && playerTurn) then f acc else acc) s (effects s)) (insertCandidates ++ [(0,expired) | null candidates])
        getMana (_,m,_,_) = m

floodFillGoal :: Bool -> Bool -> [(Int, State)] -> [(Int, State)]
floodFillGoal hardMode parity frontier = if null frontier then [] else won ++ floodFillGoal hardMode (not parity) (concatMap (updateState hardMode parity) rest)
    where
        won = filter (\(_,s) -> bosshp s <= 0) frontier
        rest = filter (\(_,s) -> hp s > 0 && bosshp s > 0) frontier

instance Show State where
    show :: State -> String
    show (State efs h m h' d') = show (map (\(b,m',d,_) -> (b,m',d)) efs,h,m,h',d')