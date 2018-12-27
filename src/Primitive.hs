module Primitive where

import Program
import Function
import Data.Map ((!))
import Data.Foldable (toList)
import qualified Control.Monad.State as M
import Debug.Trace

-- Primitive Operators

con :: Function -> [Function] -> Function
con f gs | (length gs == k) && all (\g -> argc g == n) (tail gs) =
    function  n $ \(y, xs)  ->
    context k 0 $ \(zs, []) -> do
        (_, State vars _, _) <- M.get
        mapM_ (\(g, z) -> call g (z, xs)) $ zip gs zs
        call f (y, zs)
    where k = argc f
          n = argc $ head gs

rec :: Function -> Function -> Function
rec f g | argc g == n + 2 = function (n + 1) $ \(y, t:xs) ->
    context 1 1 $ \([z], [a]) -> do
        (_, _, exit) <- M.get
        call f (y, xs)
        _label_ a
        gz t exit
        call g (y, z:y:xs)
        inc z
        dec t
        goto a
    where n = argc f

(%) :: Function -> Int -> Function -- rewrite argc
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
iota f = rec id' (con f [u (2 + argc f) 1])

(>^<) :: Function -> Value -> Function
f >^< n = unary $ \(y, [x]) -> do
    set y n
    call (iota f) (y, [y, x])

k :: Value -> Function -- Constant k
k n = con (s >^< n) [z]

rev :: Function -> Function -- Flip argument list
rev f | argc f == 2 = con f [u 2 1, u 2 0]

apply :: Function -> Value -> Function -- Function currying
apply f a = let n' = (argc f - 1); p = u n'
            in con f $ (k a % n'):map p [0..n'-1]

pass :: Function -> Int -> Function -- Pass first k parameters
pass f k = let n = argc f; p = u (k + n)
           in con f (map p [k..k+n-1])

fold :: Function -> Function -> Function -- Fold a function by its first parameter rolling
fold b f | argc b == 2 = let n = argc f; p = u (n + 1)
                         in rec (apply f 0) (con b [p 1, con f (con s [p 0]:map p [2..n])])

