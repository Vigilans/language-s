module Function where

import Program
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Control.Monad.State as M

type ProtoFunction = (Variable, [Variable]) -> Runtime Variable

type Function = [Variable] -> Runtime Variable

function :: Int -> ProtoFunction -> Function
function argv func args = do
    [out] <- freeVars 1
    let fixArgsList
            | length args < argv = freeVars (argv - length args) >>= \rest -> return (args ++ rest)
            | length args > argv = return (take argv args)
            | otherwise = return args
    args' <- fixArgsList
    func (out, args')

unary :: ProtoFunction -> Function
unary = function 1

binary :: ProtoFunction -> Function
binary = function 2

-- State Monad based program

computeFunction :: Function -> [Value] -> (Variable, [Snapshot], Program)
computeFunction func args =
    let inputs     = take (length args) (Var <$> [1..]) -- input var starts from 1
        initVars   = Map.fromList $ (Var 0, 0):(zip inputs args) -- 0 reserved for y
        initLabels = Map.fromList [(exit, -1)] -- 0 reserved for exit label
        initState  = State initVars initLabels 
        (retVar, ps) = M.runState (func inputs) ([], initState)  
    in (retVar, computation ps, fst ps)

traceFunction :: Function -> [Value] -> IO ()
traceFunction func args =
    let (_, snapshots, program) = computeFunction func args
        snapshots' = (\(i, s) -> (
            map snd $ Map.toList $ varTable s, 
            i, 
            if i < length program then program !! i else Nop
            )) <$> snapshots
    in mapM_ print snapshots'

invoke :: Function -> [Value] -> Value
invoke func args =
    let (retVar, snapshots, _) = computeFunction func args
    in (varTable . snd . last $ snapshots) ! retVar
