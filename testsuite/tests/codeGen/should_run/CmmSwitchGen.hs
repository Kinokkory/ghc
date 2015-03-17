{-# LANGUAGE TupleSections #-}

-- Generates CmmSwitch.hs

import qualified Data.Set as S
import Data.Word

output :: Integer -> Integer
output n = n`div`2 + 42

def :: Integer
def = 1337

type Spec = (String, Bool, [Integer])

primtyp True = "Int#"
primtyp False = "Word#"

con True = "I#"
con False = "W#"

hash True = "#"
hash False = "##"

primLit s v = show v ++ hash s

genSwitch :: Spec -> String
genSwitch (name, signed, values) = unlines $
  [ "{-# NOINLINE " ++ name ++ " #-}" ] ++
  [ name ++ " :: " ++ primtyp signed ++ " -> " ++ primtyp signed ] ++
  [ name ++ " " ++ primLit signed v ++ " = " ++ primLit signed (output v)
  | v <- values] ++
  [ name ++ " _ = " ++ primLit signed def ]

genCheck :: Spec -> String
genCheck (name, signed, values) = unlines $
  [ checkName name ++ " :: IO ()" ] ++
  [ checkName name ++ " = do " ] ++
  [ "  let r = " ++ con signed ++ " (" ++ name ++ " " ++ primLit signed v ++ ") in " ++ 
    "unless (r == " ++ show (output v) ++ ") $ putStrLn $ \"ERR: " ++ name ++ " (" ++ primLit signed v ++ ") is \" ++ show r ++ \" and not " ++ show (output v) ++ "\""
  | v <- values]

checkName :: String -> String
checkName f = f ++ "_check"

genMain :: [Spec] -> String
genMain specs = unlines $ "main = do" : [ "    " ++ checkName n | (n,_,_) <- specs ]

genMod :: [Spec] -> String
genMod specs = unlines $
    "{-# LANGUAGE MagicHash #-}" :
    "import Control.Monad (unless)" :
    "import GHC.Exts" :
    map genSwitch specs ++
    map genCheck specs ++
    [ genMain specs ]
    
main = putStrLn $
    genMod $ zipWith (\n (s,v) -> (n,s,v)) names $ signedChecks ++ unsignedChecks


signedChecks :: [(Bool, [Integer])]
signedChecks = map (True,)
    [ [1..10]
    , [0..10]
    , [-1..10]
    , [1..3]
    , [1..4]
    , [1..5]
    , [minS,0,maxS]
    , [minS..minS+10]++[maxS-10 .. maxS]
    ]

minU, maxU, minS, maxS :: Integer
minU = 0
maxU = fromIntegral (maxBound :: Word)
minS = fromIntegral (minBound :: Int)
maxS = fromIntegral (maxBound :: Int)


unsignedChecks :: [(Bool, [Integer])]
unsignedChecks = map (False,) [
    ]

names :: [String]
names = [ c1:c2:[] | c1 <- ['a'..'z'], c2 <- ['a'..'z']]
