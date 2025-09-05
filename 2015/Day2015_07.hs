module Day2015_07 where
import Data.Word (Word16)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.Bits ( Bits(shiftR, complement, (.&.), (.|.), shiftL) )
import qualified Data.HashMap.Lazy as HM

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/07.txt"
    -- part 1
    let circuit = HM.fromList $ map parseLine $ lines contents
        Just a = HM.lookup "a" $ resolveCircuit circuit
    print a
    -- part 2
    print $ fromJust $ HM.lookup "a" $ resolveCircuit $ HM.insert "b" (Const (Num a)) circuit

data Reg = Num Word16 | Reg String
data Op = Const Reg | Not Reg | Add Reg Reg | Or Reg Reg | Lshift Reg Reg | Rshift Reg Reg

parseLine :: String -> (String, Op)
parseLine line = case words line of
    [a,_,x] -> (x,Const (read' a))
    ["NOT",a,_,x] -> (x,Not (read' a))
    [x,"AND",y,_,z] -> (z,Add (read' x) (read' y))
    [x,"OR",y,_,z] -> (z,Or (read' x) (read' y))
    [x,"LSHIFT",y,_,z] -> (z,Lshift (read' x) (read' y))
    [x,"RSHIFT",y,_,z] -> (z,Rshift (read' x) (read' y))
    where
        read' x = case readMaybe x of
            Just n -> Num n
            Nothing -> Reg x

resolveCircuit :: HM.HashMap String Op -> HM.HashMap String Word16
resolveCircuit lookup' = final
    where
        final = HM.map resolve lookup'
        resolve (Const r) = case r of
            Num n -> n
            Reg s -> fromJust $ HM.lookup s final
        resolve (Not r) = complement $ resolve (Const r)
        resolve (Add a b) = resolve (Const a) .&. resolve (Const b)
        resolve (Or a b) = resolve (Const a) .|. resolve (Const b)
        resolve (Lshift a b) = shiftL (resolve (Const a)) (fromIntegral $ resolve (Const b))
        resolve (Rshift a b) = shiftR (resolve (Const a)) (fromIntegral $ resolve (Const b))