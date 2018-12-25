module Primitive where

import Program
import Function
import Data.Function (fix)
import qualified Control.Monad.State as M

con :: Function -> [Function] -> Function
con f gs | (length gs == k) && all (\g -> argv g == n) (tail gs) =
    function n $ \(y, xs) -> do
        zs <- freeVars k
        mapM_ (\(g, z) -> call g (z, xs)) $ zip gs zs
        call f (y, zs)
        ret y
    where k = argv f
          n = argv $ head gs

rec :: Function -> Function -> Function
rec f g | argv g == n + 2 =
    function (n + 1) $ \(y, t : xs) -> do
        (_, _, exit) <- M.get
        [z] <- freeVars 1
        [a] <- freeLabels 1
        call f (y, xs)
        _label_ a
        gz t exit
        call g (y, z:y:xs)
        inc z
        dec t
        goto a
        ret y
    where n = argv f

s :: Function
s = unary $ \(y, [x]) -> do
    inc x
    ret x

n :: Function
n = unary $ \(_, _) -> ret false

u :: Int -> Int -> Function
u n i = function n $ \(y, xs) -> ret $ xs !! i
