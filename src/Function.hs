module Function where

import Program
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Control.Monad.State as M

type Signature = (Variable, [Variable], Label)

type Function = Signature -> Runtime Variable

function :: Int -> Function -> Function
function argv func (out, args, exit) = do
    (p, State vars labels) <- M.get
    let vars'  = Map.insertWith (const id) out 0 vars
        vars'' = foldl (\vs v -> Map.insertWith (const id) v 0 vs) vars' args
    M.put (p, State vars'' labels)
    let fixInputList
            | length args < argv = freeVars (argv - length args) >>= \rest -> return (args ++ rest)
            | length args > argv = return (take argv args)
            | otherwise = return args
    ins <- fixInputList
    func (out, ins, exit)

unary :: Function -> Function
unary = function 1

binary :: Function -> Function
binary = function 2

ternary :: Function -> Function
ternary = function 3

-- State Monad based program

computeFunction :: Function -> [Value] -> (Variable, [Snapshot], Program)
computeFunction func args =
    let inputs     = take (length args) (Var <$> [1..]) -- >= 1 for xs, 0 for y
        emptyState = State Map.empty Map.empty
        signature  = (Var 0, inputs, Label (-1)) -- goto -1 will terminate program
        (output, (p, State vs ls)) = M.runState (func signature) ([], emptyState)
        constants  = [(true, 1), (false, 0)]
        initVars   = Map.fromList $ zip inputs args ++ constants
        initState  = State (Map.union initVars vs) ls -- feed state with inputs
    in (output, computation (p, initState) , p)

traceFunction :: Function -> [Value] -> IO ()
traceFunction func args =
    let (_, snapshots, program) = computeFunction func args
        snapshots' = (\(i, s) -> (
            map snd . filter ((>=Var 0).fst) . Map.toList . varTable $ s,
            i,
            if i < length program then program !! i else Nop
            )) <$> snapshots
    in mapM_ print snapshots'

invoke :: Function -> [Value] -> Value -- inovke as true function
invoke func args =
    let (retVar, snapshots, _) = computeFunction func args
    in (varTable . snd . last $ snapshots) ! retVar

call :: Function -> (Variable, [Variable]) -> Runtime Address
call func (out, ins) = do
    (y:xs) <- freeVars (1 + length ins)
    [e] <- freeLabels 1
    mapM_ (uncurry mov) $ zip xs ins
    func (y, xs, e)
    _label_ e
    mov out y
