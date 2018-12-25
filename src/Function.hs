module Function where

import Program
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Control.Monad.State as M

type Signature = (Variable, [Variable])

type FuncImpl = Signature -> Runtime Variable

data Function = Function { argv :: Int, func :: FuncImpl }

function :: Int -> FuncImpl -> Function
function argv func = Function argv $ \(out, args) -> do
    (p, State vars labels, e) <- M.get
    let vars'  = Map.insertWith (const id) out 0 vars
        vars'' = foldl (\vs v -> Map.insertWith (const id) v 0 vs) vars' args
    M.put (p, State vars'' labels, e)
    let fixInputList
            | length args < argv = freeVars (argv - length args) >>= \rest -> return (args ++ rest)
            | length args > argv = return (take argv args)
            | otherwise = return args
    ins <- fixInputList
    func (out, ins)

unary :: FuncImpl -> Function
unary = function 1

binary :: FuncImpl -> Function
binary = function 2

ternary :: FuncImpl -> Function
ternary = function 3

-- State Monad based program

computeFunction :: Function -> [Value] -> (Variable, [Snapshot], Program)
computeFunction (Function _ func) args =
    let inputs     = take (length args) (Var <$> [1..]) -- >= 1 for xs, 0 for y
        signature  = (Var 0, inputs) -- goto -1 will terminate program
        emptyState = ([], State Map.empty Map.empty, Label (-1))
        (output, (p, State vs ls, _)) = M.runState (func signature) emptyState
        constants  = [(true, 1), (false, 0)]
        initVars   = Map.fromList $ zip inputs args ++ constants
        initState  = State (Map.union initVars vs) ls -- feed state with inputs
    in (output, computation p initState, p)

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
call (Function _ func) (out, ins) = do
    (_, _, exit) <- M.get
    (y:xs) <- freeVars (1 + length ins)
    [e]    <- freeLabels 1
    mapM_ (uncurry mov) $ zip xs ins
    _exit_ e
    result <- func (y, xs)
    _label_ e
    mov out result
    _exit_ exit

ret :: Variable -> Runtime Variable
ret out = do
    (_, _, exit) <- M.get
    goto exit
    return out
