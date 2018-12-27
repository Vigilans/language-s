module Function where

import Program
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Control.Monad.State as M
import Debug.Trace

type Signature = (Variable, [Variable])

data Function = Function { argc :: Int, func :: Signature -> Runtime Address }

function :: Int -> (Signature -> Runtime Address) -> Function
function argc func = Function argc $ \(out, args) -> context 0 0 $ \_ -> do
    (p, State vars labels, exit) <- M.get  
    let vars'  = Map.insertWith (const id) out 0 vars
        vars'' = foldl (\vs v -> Map.insertWith (const id) v 0 vs) vars' args
    M.put (p, State vars'' labels, exit)
    rest <- freeVars $ max (argc - length args) 0
    func (out, args ++ rest)

nullary :: (Signature -> Runtime Address) -> Function
nullary = function 0

unary :: (Signature -> Runtime Address) -> Function
unary = function 1

binary :: (Signature -> Runtime Address) -> Function
binary = function 2

ternary :: (Signature -> Runtime Address) -> Function
ternary = function 3

-- State Monad based program

computeFunction :: Function -> [Value] -> ([Snapshot], Program)
computeFunction (Function _ func) args =
    let inputs     = take (length args) (Var <$> [1..]) -- input starts from 1
        signature  = (Var 0, inputs) -- goto -1 will terminate program
        emptyState = ([], State Map.empty Map.empty, Label (-1))
        (p, State vs ls, _) = M.execState (func signature) emptyState
        constants  = [(Var 0, 0), (true, 1), (false, 0)]
        initVars   = Map.fromList $ zip inputs args ++ constants
        initState  = State (Map.union initVars vs) ls -- feed state with inputs
    in (computation p initState, p)

traceFunction :: Function -> [Value] -> IO ()
traceFunction func args =
    let (snapshots, program) = computeFunction func args
        snapshots' = (\(i, s) -> (
            map snd . filter ((>=Var 0).fst) . Map.toList . varTable $ s,
            i - 1,
            if i > 0 then program !! (i - 1) else Nop
            )) <$> snapshots
    in mapM_ print snapshots'

invoke :: Function -> [Value] -> Value -- inovke as true function
invoke func args =
    let (snapshots, _) = computeFunction func args
    in (varTable . snd . last $ snapshots) ! Var 0 -- 0 reserved for output

call :: Function -> (Variable, [Variable]) -> Runtime Address
call (Function _ func) (out, ins) = context (1 + length ins) 0 $ \(y:xs, []) -> do
    clr y
    mapM_ (uncurry mov) $ zip xs ins
    func (y, xs) -- exit label is set inside func
    mov out y

ret :: Runtime Address
ret = Program.exit
