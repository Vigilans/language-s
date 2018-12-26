module Primitive where

import Program
import Function
import Data.Map ((!))
import Data.Foldable (toList)
import qualified Control.Monad.State as M
import Debug.Trace

-- Primitive Operators

con :: Function -> [Function] -> Function
con f gs | (length gs == k) && all (\g -> argv g == n) (tail gs) =
    function n $ \(y, xs) -> context k 0 $ \(zs, []) -> do
        (_, State vars _, _) <- M.get
        mapM_ (\(g, z) -> call g (z, xs)) $ zip gs zs
        call f (y, zs)
        curAddr
    where k = argv f
          n = argv $ head gs

rec :: Function -> Function -> Function
rec f g | argv g == n + 2 = function (n + 1) $ \(y, t : xs) ->
    context 1 1 $ \([z], [a]) -> do
        (_, _, exit) <- M.get
        call f (y, xs)
        _label_ a
        gz t exit
        call g (y, z : y : xs)
        addr <- curAddr
        inc z
        dec t
        goto a
    where n = argv f

(%) :: Function -> Int -> Function -- rewrite argv
(Function _ f) % n = Function n f

-- Initial Functions

z :: Function
z = nullary $ \(y, _) -> mov y false

s :: Function
s = unary $ \(y, [x]) -> inc x >> mov y x

u :: Int -> Int -> Function
u n i = function n $ \(y, xs) -> mov y $ xs !! i

-- Basic Functions and Operators

id' :: Function
id' = u 1 0

iota :: Function -> Function -- Nth iteration of f
iota f = rec id' (con f [u (2 + argv f) 1])

(^) :: Function -> Value -> Function
f ^ n = unary $ \(y, [x]) -> do
    set y n
    call (iota s) (y, [y, x])

k :: Value -> Function -- Constant k
k n = con (s Primitive.^ n) [z]

rev :: Function -> Function -- Flip argument list
rev f | argv f == 2 = con f [u 2 1, u 2 0]
