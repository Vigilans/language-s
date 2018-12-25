{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Program where

-- import Data.List
import Data.Map ((!), notMember)
import qualified Data.Map as Map
import qualified Control.Monad.State as M

-- Statement list:
-- V <- V
-- V <- V + 1
-- V <- V - 1
-- IF V /= 0 GOTO L 

newtype Variable = Var Int deriving (Show, Eq, Ord)

type Value = Integer

newtype Label = Label Int deriving (Show, Eq, Ord)

type Address = Int

data Instruction = Nop | Inc Variable | Dec Variable | Gnz Variable Label deriving (Show)

type Program = [Instruction]

data State = State {
    varTable :: Map.Map Variable Value,
    labelTable :: Map.Map Label Address
} deriving (Show)

type Snapshot = (Int, State)

type ProgramState = (Program, State)

successor :: Program -> Snapshot -> Snapshot
successor program (i, State vars labels)
    | i == t = (t, s)
    | otherwise = case program !! i of
        Nop   -> (i + 1, State vars labels)
        Inc v -> (i + 1, State (Map.update (\val -> Just (val + 1)) v vars) labels)
        Dec v -> (i + 1, State (Map.update (\val -> Just (max (val - 1) 0)) v vars) labels)
        Gnz v l | vars ! v == 0      -> (i + 1, s)
                | notMember l labels -> (t, s)
                | labels ! l < 0     -> (t, s)
                | labels ! l >= t    -> (t, s)
                | otherwise          -> (labels ! l, s)
    where s = State vars labels
          t = length program

computation :: ProgramState -> [Snapshot] -- equivalent as "takeUntil"
computation (p, s) = foldr (\x ys -> if fst x /= length p then x:ys else [x]) [] $ iterate (successor p) (0, s)

-- Monad instructions

type Runtime = M.State ProgramState

appendIr :: Instruction -> ProgramState -> (Address, ProgramState)
appendIr ir (p, s) = (length p, (p ++ [ir] , s)) -- TODO: append to last effiently

_label_ :: Label -> Runtime Address
_label_ l = M.state $ \(p, State vs ls) ->
    let addr = length p
    in (addr, (p, State vs (Map.insert l addr ls)))

nop :: Runtime Address
nop = M.state $ appendIr Nop

inc :: Variable -> Runtime Address
inc v = M.state $ appendIr $ Inc v

dec :: Variable -> Runtime Address
dec v = M.state $ appendIr $ Dec v

gnz :: Variable -> Label -> Runtime Address
gnz v l = M.state $ appendIr $ Gnz v l

-- Useful tools for writing program

freeVars :: Int -> Runtime [Variable]
freeVars n = M.state $ \(p, State vars labels) ->
    let firstFree = case Map.keys vars of
            [] -> 0
            vs -> (\(Var n) -> n) (maximum $ Map.keys vars) + 1
        newVars = take n (Var <$> [firstFree..])
    in (newVars, (p, State (foldl (\vs v -> Map.insert v 0 vs) vars newVars) labels))

freeLabels :: Int -> Runtime [Label]
freeLabels n = M.state $ \(p, State vars labels) ->
    let firstFree = case Map.keys labels of
            [] -> 0
            ls -> (\(Label n) -> n) (maximum $ Map.keys labels) + 1
        newLabels = take n (Label <$> [firstFree..])
    in (newLabels, (p, State vars (foldl (\ls l -> Map.insert l (-1) ls) labels newLabels)))

curAddr :: Runtime Address
curAddr = M.state $ \s -> (length $ fst s, s)

-- Basic Monad Macros

exit :: Label
exit = Label 0

goto :: Label -> Runtime Address
goto l = do
    [z] <- freeVars 1
    inc z
    gnz z l

clr :: Variable -> Runtime Address
clr v = do
    [l] <- freeLabels 1
    _label_ l
    dec v
    gnz v l

asgn :: Variable -> Variable -> Runtime Address
asgn y x = do
    [z] <- freeVars 1
    [a, b, c, d, e] <- freeLabels 5
    clr y
    _label_ a
    gnz x b
    goto c
    _label_ b
    dec x
    inc y
    inc z
    goto a
    _label_ c
    gnz z d
    goto e
    _label_ d
    dec z
    inc x
    goto c
    _label_ e
