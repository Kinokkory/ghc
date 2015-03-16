{-# LANGUAGE GADTs #-}
module CmmSwitch (
     SwitchTargets,
     mkSwitchTargets,
     switchTargetsCases, switchTargetsDefault, switchTargetsRange, switchTargetsSigned,
     mapSwitchTargets, switchTargetsToTable, switchTargetsFallThrough,
     switchTargetsToList, eqSwitchTargetWith,

     SwitchPlan(..),
     createSwitchPlan,
  ) where

import Outputable
import Compiler.Hoopl

import Data.Maybe
import Data.List (groupBy)
import Data.Function (on)
import qualified Data.Map as M

-- Note [Cmm Switches, the general plan]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Compiling a high-level switch statement, as it comes out of a STG case
-- expression, for example, allows for a surprising amount of design decisions.
-- Therefore, we cleanly separated this from the Stg → Cmm transformation, as
-- well as from the actual code generation.
--
-- The overall plan is:
--  * The Stg → Cmm transformation creates a single `SwitchTargets` in
--    emitSwitch and emitCmmLitSwitch in StgCmmUtils.hs.
--    At this stage, they are unsuitable for code generation.
--  * A dedicated Cmm transformation (CmmCreateSwitchPlans) replaces these
--    switch statements with code that is suitable for code generation, i.e.
--    a nice balanced tree of decisions with dense jump tables in the leafs.
--    The actual planning of this tree is performed in pure code in createSwitchPlan
--    in this module. See Note [createSwitchPlan].
--  * The actual code generation will not do any further processing and
--    implement each CmmSwitch with a jump tables.
--
-- When compiling to LLVM or C, CmmCreateSwitchPlans leaves the switch
-- statements alone, as we can turn a SwitchTargets value into a nice
-- switch-statement in LLVM resp. C, and leave the rest to the compiler.

-----------------------------------------------------------------------------
-- Magic Constants
--
-- There are a lot of heuristics here that depend on magic values where it is
-- hard to determine the "best" value (for whatever that means). These are the
-- magic values:

-- | Number of consecutive default values allowed in a jump table. If there are
-- more of them, the jump tables are split.
-- Currently 10, for no particular good reason.
maxJumpTableHole :: Integer
maxJumpTableHole = 10

-- | Minimum size of a jump table. If the number is smaller, the switch is
-- implemented using conditionals.
-- Currently 5, because an if-then-else tree of 4 values is nice and compact.
minJumpTableSize :: Int
minJumpTableSize = 5

-- | Minimum non-zero offset for a jump table. See Note [Jump Table Offset].
minJumpTableOffset :: Integer
minJumpTableOffset = 2


-----------------------------------------------------------------------------
-- Switch Targets

-- Note [SwitchTargets]:
-- ~~~~~~~~~~~~~~~~~~~~~
--
-- The branches of a switch are stored in a SwitchTargets, which consists of an
-- (optional) default jump target, and a map from values to jump targets.
--
-- If the default jump target is absent, the behaviour of the switch outside the
-- values of the map is undefined.
--
-- We use an Integer for the keys the map so that it can be used in switches on
-- unsigned as well as signed integers.
--
-- The map must not be empty.
--
-- Before code generation, the table needs to be brought into a form where all
-- entries are non-negative, so that it can be compiled into a jump table.
-- See switchTargetsToTable.


-- See Note [SwitchTargets]
data SwitchTargets =
    SwitchTargets
        Bool                       -- ^ Signed values
        (Maybe (Integer, Integer)) -- ^ Range
        (Maybe Label)              -- ^ Default value
        (M.Map Integer Label)      -- ^ The branches
    deriving (Show, Eq)

-- mkSwitchTargets normalises the map a bit:
--  * No entries outside the range
--  * No entries equal to the default
--  * No default if there is a range, and all elements have explicit values 
mkSwitchTargets :: Bool -> Maybe (Integer, Integer) -> Maybe Label -> M.Map Integer Label -> SwitchTargets
mkSwitchTargets signed mbrange mbdef ids
    = SwitchTargets signed mbrange mbdef' ids' 
  where
    ids' = dropDefault $ restrict ids
    mbdef' | defaultNeeded = mbdef
           | otherwise     = Nothing

    -- It drops entries outside the range, if there is a range
    restrict | Just (lo,hi) <- mbrange = M.filterWithKey (\x _ -> lo <= x && x <= hi)
             | otherwise               = id

    -- It drops entries that equal the default, if there is a default
    dropDefault | Just l <- mbdef = M.filter (/= l)
                | otherwise       = id

    -- Check if the default is still needed
    defaultNeeded | Just (lo,hi) <- mbrange = fromIntegral (M.size ids') /= hi-lo+1
                  | otherwise = True


mapSwitchTargets :: (Label -> Label) -> SwitchTargets -> SwitchTargets
mapSwitchTargets f (SwitchTargets signed range mbdef branches)
    = SwitchTargets signed range (fmap f mbdef) (fmap f branches)

switchTargetsCases :: SwitchTargets -> [(Integer, Label)]
switchTargetsCases (SwitchTargets _ _ _ branches) = M.toList branches

switchTargetsDefault :: SwitchTargets -> Maybe Label
switchTargetsDefault (SwitchTargets _ _ mbdef _) = mbdef

switchTargetsRange :: SwitchTargets -> Maybe (Integer, Integer)
switchTargetsRange (SwitchTargets _ mbrange _ _) = mbrange

switchTargetsSigned :: SwitchTargets -> Bool
switchTargetsSigned (SwitchTargets signed _ _ _) = signed

-- switchTargetsToTable creates a dense jump table, usable for code generation.
-- This is not possible if there is no explicit range, so before code generation
-- all switch statements need to be transformed to one with an explicit range.
--
-- Returns an offset to add to the value; the list is 0-based on the result
--
-- TODO: Is the conversion from Integral to Int fishy?
switchTargetsToTable :: SwitchTargets -> (Int, [Maybe Label])
switchTargetsToTable (SwitchTargets _ Nothing _mbdef _branches)
    = pprPanic "switchTargetsToTable" empty
switchTargetsToTable (SwitchTargets _ (Just (lo,hi)) mbdef branches)
    = (fromIntegral (-start), [ labelFor i | i <- [start..hi] ])
  where
    labelFor i = case M.lookup i branches of Just l -> Just l
                                             Nothing -> mbdef
    start | lo >= 0 && lo < minJumpTableOffset  = 0  -- See Note [Jump Table Offset]
          | otherwise                           = lo

-- Note [Jump Table Offset]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Usually, the code for a jump table starting at x will first subtract x from
-- the value, to avoid a large amount of empty entries. But if x is very small,
-- the extra entries are no worse than the subtraction in terms of code size, and
-- not having to do the subtraction is quicker.
--
-- I.e. instead of
--     _u20N:
--             leaq -1(%r14),%rax
--             jmp *_n20R(,%rax,8)
--     _n20R:
--             .quad   _c20p
--             .quad   _c20q
-- do
--     _u20N:
--             jmp *_n20Q(,%r14,8)
--
--     _n20Q:
--             .quad   0
--             .quad   _c20p
--             .quad   _c20q
--             .quad   _c20r

switchTargetsToList :: SwitchTargets -> [Label]
switchTargetsToList (SwitchTargets _ _ mbdef branches)
    = maybeToList mbdef ++ M.elems branches

-- | Groups cases with equal targets, suitable for pretty-printing to a
-- c-like switch statement with fall-through semantics.
switchTargetsFallThrough :: SwitchTargets -> ([([Integer], Label)], Maybe Label)
switchTargetsFallThrough (SwitchTargets _ _ mbdef branches) = (groups, mbdef)
  where
    groups = map (\xs -> (map fst xs, snd (head xs))) $
             groupBy ((==) `on` snd) $
             M.toList branches

eqSwitchTargetWith :: (Label -> Label -> Bool) -> SwitchTargets -> SwitchTargets -> Bool
eqSwitchTargetWith eq (SwitchTargets signed1 range1 mbdef1 ids1) (SwitchTargets signed2 range2 mbdef2 ids2) =
    signed1 == signed2 && range1 == range2 && goMB mbdef1 mbdef2 && goList (M.toList ids1) (M.toList ids2)
  where
    goMB Nothing Nothing = True
    goMB (Just l1) (Just l2) = l1 `eq` l2
    goMB _ _ = False
    goList [] [] = True
    goList ((i1,l1):ls1) ((i2,l2):ls2) = i1 == i2 && l1 `eq` l2 && goList ls1 ls2
    goList _ _ = False

-----------------------------------------------------------------------------
-- Code generation for Switches


-- Note [createSwitchPlan]
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- A SwitchPlan describes how a Switch statement is to be broken down into
-- smaller pieces suitable for code generation.
--
-- createSwitchPlan creates such a switch plan, in these steps:
--  1. it checks if the switch is unbounded, and takes care of the default
--     segments outside the range of the actual branches (addRange)
--  2. it splits the switch statement at segments of non-default values that
--     are too large. See splitAtHoles and Note [When to split SwitchTargets]
--  3. Too small jump tables should be avoided, so we break up smaller pieces
--     in breakTooSmall.
--  4. We will in the segments between those pieces with a jump to the default
--     label (if there is one), returning a SeparatedList in mkFlatSwitchPlan
--  5. The segments outside the range from step 1 are added now.
--  6. We find replace two less-than branches by a single equal-to-test in
--     findSingleValues
--  7. The thus collected pieces are assembled to a balanced binary tree.

data SwitchPlan
    = Unconditionally Label 
    | IfEqual Integer Label SwitchPlan
    | IfLT Bool Integer SwitchPlan SwitchPlan
    | JumpTable SwitchTargets
  deriving Show

type FlatSwitchPlan = SeparatedList Integer SwitchPlan

createSwitchPlan :: SwitchTargets -> SwitchPlan
createSwitchPlan ids = 
    -- pprTrace "createSwitchPlan" (text (show ids) $$ text (show (range,m)) $$ text (show pieces) $$ text (show flatPlan) $$ text (show plan)) $
    plan 
  where
    signed = switchTargetsSigned ids
    (range, m, wrap) = addRange signed ids
    pieces = concatMap breakTooSmall $ splitAtHoles maxJumpTableHole m
    flatPlan = findSingleValues $ wrap $ mkFlatSwitchPlan signed (switchTargetsDefault ids) range pieces
    plan = buildTree signed $ flatPlan


---
--- Step 1: Adding a range
---

-- All switch targets surviving this stage needs a range. This adds the range,
-- together with the neccessary branching.
addRange :: Bool -> SwitchTargets ->
    ((Integer, Integer), M.Map Integer Label, FlatSwitchPlan -> FlatSwitchPlan)

-- There is a range, nothing to do
addRange _ (SwitchTargets _ (Just r) _ m) = (r, m, id)

-- There is no range, but also no default. We can set the range
-- to whatever is found in the map
addRange _ (SwitchTargets _ Nothing Nothing m) = ((lo,hi), m, id)
  where (lo,_) = M.findMin m
        (hi,_) = M.findMax m

-- No range, but a default. Create a range, but also emit SwitchPlans for
-- outside the range.
addRange signed (SwitchTargets _ Nothing (Just l) m)
    = ( (lo,hi)
      , m
      , \plan -> lower_chunk plan `snocSL` (hi+1, Unconditionally l)
      )
  where (lo,_) = M.findMin m
        (hi,_) = M.findMax m
        lower_chunk = if not signed && lo == 0 then id else
             ((Unconditionally l, lo) `consSL`)


---
--- Step 2: Splitting at large holes
---
splitAtHoles :: Integer -> M.Map Integer a -> [M.Map Integer a]
splitAtHoles holeSize m = map (\range -> restrictMap range m) nonHoles
  where
    holes = filter (\(l,h) -> h - l > holeSize) $ zip (M.keys m) (tail (M.keys m))
    nonHoles = reassocTuples lo holes hi

    (lo,_) = M.findMin m
    (hi,_) = M.findMax m

---
--- Step 3: Avoid small jump tables
---
-- We do not want jump tables below a certain size. This breaks them up
-- (into singleton maps, for now)
breakTooSmall :: M.Map Integer a -> [M.Map Integer a]
breakTooSmall m
  | M.size m > minJumpTableSize = [m]
  | otherwise                   = [M.singleton k v | (k,v) <- M.toList m]

---
---  Step 4: Fill in the blanks
---

-- A FlatSwitchPlan is a list of SwitchPlans, seperated by a integer dividing the range.
-- So if we have  [plan1] n [plan2], then we use plan1 if the expression is <
-- n, and plan2 otherwise.

mkFlatSwitchPlan :: Bool -> Maybe Label -> (Integer, Integer) -> [M.Map Integer Label] -> FlatSwitchPlan

-- If we have no default (i.e. undefined where there is no entry), we can
-- branch at the minimum of each map
mkFlatSwitchPlan _ Nothing _ [] = pprPanic "mkFlatSwitchPlan with nothing left to do" empty
mkFlatSwitchPlan signed  Nothing _ (m:ms)
  = (mkLeafPlan signed Nothing m , [ (fst (M.findMin m'), mkLeafPlan signed Nothing m') | m' <- ms ])

-- If we have a default, we have to interleave segments that jump 
-- to the default between the maps
mkFlatSwitchPlan signed (Just l) r ms = let ((_,p1):ps) = go r ms in (p1, ps)
  where
    go (lo,hi) []
        | lo > hi = []
        | otherwise = [(lo, Unconditionally l)]
    go (lo,hi) (m:ms)
        | lo < min
        = (lo, Unconditionally l) : go (min,hi) (m:ms)
        | lo == min
        = (lo, mkLeafPlan signed (Just l) m) : go (max+1,hi) ms
        | otherwise
        = pprPanic "mkFlatSwitchPlan" (integer lo <+> integer min)
      where
        min = fst (M.findMin m)
        max = fst (M.findMax m)
    

mkLeafPlan :: Bool -> Maybe Label -> M.Map Integer Label -> SwitchPlan
mkLeafPlan signed mbdef m
    | [(_,l)] <- M.toList m -- singleton map
    = Unconditionally l 
    | otherwise
    = JumpTable $ mkSwitchTargets signed (Just (min,max)) mbdef m
  where
    min = fst (M.findMin m)
    max = fst (M.findMax m)

---
---  Step 5: Reduce the number of branches using ==
---

-- A seqence of three unconditional jumps, with the outer two pointing to the
-- same value and the bounds off by exactly one can be improved 
findSingleValues :: FlatSwitchPlan -> FlatSwitchPlan
findSingleValues (Unconditionally l, (i, Unconditionally l2) : (i', Unconditionally l3) : xs)
  | l == l3 && i + 1 == i'
  = findSingleValues (IfEqual i l2 (Unconditionally l), xs)
findSingleValues (p, (i,p'):xs)
  = (p,i) `consSL` findSingleValues (p', xs)
findSingleValues (p, [])
  = (p, [])

---
---  Step 6: Actually build the tree
---

-- Build a balanced tree from a separated list
buildTree :: Bool -> FlatSwitchPlan -> SwitchPlan
buildTree _ (p,[]) = p
buildTree signed sl = IfLT signed m (buildTree signed sl1) (buildTree signed sl2)
  where 
    (sl1, m, sl2) = divideSL sl



--
-- Utility data type: Non-empty lists with extra markers in between each
-- element:
--

type SeparatedList b a = (a, [(b,a)])

consSL :: (a, b) -> SeparatedList b a -> SeparatedList b a
consSL (a, b) (a', xs) = (a, (b,a'):xs)

snocSL :: SeparatedList b a -> (b, a) -> SeparatedList b a
snocSL (a', xs) (b, a) = (a', xs ++ [(b,a)])

divideSL :: SeparatedList b a -> (SeparatedList b a, b, SeparatedList b a)
divideSL (_,[]) = error "divideSL: Singleton SeparatedList"
divideSL (p,xs) = ((p, xs1), m, (p', xs2))
  where
    (xs1, (m,p'):xs2) = splitAt (length xs `div` 2) xs

--
-- Other Utilities
--

restrictMap :: Integral a => (a,a) -> M.Map a b -> M.Map a b
restrictMap (lo,hi) m = mid
  where (_,   mid_hi) = M.split (lo-1) m
        (mid, _) =      M.split (hi+1) mid_hi

-- for example: reassocTuples a [(b,c),(d,e)] f == [(a,b),(c,d),(e,f)]
reassocTuples :: a -> [(a,a)] -> a -> [(a,a)]
reassocTuples initial [] last
    = [(initial,last)]
reassocTuples initial ((a,b):tuples) last
    = (initial,a) : reassocTuples b tuples last
  
