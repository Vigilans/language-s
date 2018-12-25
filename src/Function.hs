module Function where

import Program
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Control.Monad.State as M
import Debug.Trace

type Signature = (Variable, [Variable])

data Function = Function { argv :: Int, func :: Signature -> Runtime Address }

function :: Int -> (Signature -> Runtime Variable) -> Function
function argv func = Function argv $ \(out, args) -> do
    (p, State vars labels, exit) <- M.get
    let vars'  = Map.insertWith (const id) out 0 vars
        vars'' = foldl (\vs v -> Map.insertWith (const id) v 0 vs) vars' args
    M.put (p, State vars'' labels, exit)
    let fixInputList
            | length args < argv = freeVars (argv - length args) >>= \rest -> return (args ++ rest)
            | length args > argv = return (take argv args)
            | otherwise = return args
    ins <- fixInputList
    [e] <- freeLabels 1
    _exit_ e
    out' <- func (out, ins)
    _label_ e
    mov out out'
    _exit_ exit

unary :: (Signature -> Runtime Variable) -> Function
unary = function 1

binary :: (Signature -> Runtime Variable) -> Function
binary = function 2

ternary :: (Signature -> Runtime Variable) -> Function
ternary = function 3

-- State Monad based program

computeFunction :: Function -> [Value] -> ([Snapshot], Program)
computeFunction (Function _ func) args =
    let inputs     = take (length args) (Var <$> [1..]) -- input starts from 1
        signature  = (Var 0, inputs) -- goto -1 will terminate program
        emptyState = ([], State Map.empty Map.empty, Label (-1))
        (p, State vs ls, _) = M.execState (func signature) emptyState
        constants  = [(true, 1), (false, 0)]
        initVars   = Map.fromList $ zip inputs args ++ constants
        initState  = State (Map.union initVars vs) ls -- feed state with inputs
    in (computation p initState, p)

traceFunction :: Function -> [Value] -> IO ()
traceFunction func args =
    let (snapshots, program) = computeFunction func args
        snapshots' = (\(i, s) -> (
            map snd . filter ((>=Var 0).fst) . Map.toList . varTable $ s,
            i,
            if i < length program then program !! i else Nop
            )) <$> snapshots
    in mapM_ print snapshots'

invoke :: Function -> [Value] -> Value -- inovke as true function
invoke func args =
    let (snapshots, _) = computeFunction func args
    in (varTable . snd . last $ snapshots) ! Var 0 -- 0 reserved for output

call :: Function -> (Variable, [Variable]) -> Runtime Address
call (Function _ func) (out, ins) = do
    (_, _, exit) <- M.get
    (y:xs) <- freeVars (1 + length ins)
    mapM_ (uncurry mov) $ zip xs ins
    func (y, xs) -- exit label is set inside func
    mov out y

ret :: Variable -> Runtime Variable
ret out = do
    (_, _, exit) <- M.get
    goto exit
    return out
