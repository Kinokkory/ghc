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
    "unless (r == " ++ show (f v) ++ ") $ putStrLn $ \"ERR: " ++ name ++ " (" ++ primLit signed v ++ ") is \" ++ show r ++ \" and not " ++ show (f v) ++ "\""
  | v <- checkValues]
  where
    f x | x `S.member` range = output x
        | otherwise          = def
    range = S.fromList values
    checkValues = S.toList $ S.fromList $
        [ v' | v <- values, v' <- [v-1,v,v+1],
               if signed then v' >= minS && v' <= maxS else v' >= minU && v' <= maxU ]

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
    , [1..3]
    , [1..4]
    , [1..5]
    , [-1..10]
    , [-10..10]
    , [-20.. -10]++[0..10]
    , [-20.. -10]++[1..10]
    , [minS,0,maxS]
    , [maxS-10 .. maxS]
    , [minS..minS+10]++[maxS-10 .. maxS]
    ]

minU, maxU, minS, maxS :: Integer
minU = 0
maxU = fromIntegral (maxBound :: Word)
minS = fromIntegral (minBound :: Int)
maxS = fromIntegral (maxBound :: Int)


unsignedChecks :: [(Bool, [Integer])]
unsignedChecks = map (False,)
    [ [0..10]
    , [1..10]
    , [0]
    , [0..1]
    , [0..2]
    , [0..3]
    , [0..4]
    , [1]
    , [1..2]
    , [1..3]
    , [1..4]
    , [1..5]
    , [minU,maxU]
    , [maxU-10 .. maxU]
    , [minU..minU+10]++[maxU-10 .. maxU]
    ]

names :: [String]
names = [ c1:c2:[] | c1 <- ['a'..'z'], c2 <- ['a'..'z']]
