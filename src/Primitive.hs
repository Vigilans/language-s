module Primitive where

import Program
import Function
import Data.Sequence (Seq(..), (<|))
import Data.Foldable (toList)
import qualified Data.Sequence as S
import qualified Control.Monad.State as M

-- Operators

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
    function (n + 1) $ \(y, xx) -> do
        let (xs :|> t) = S.fromList xx
        (_, _, exit) <- M.get
        [z] <- freeVars 1
        [a] <- freeLabels 1
        call f (y, toList xs)
        _label_ a
        gz t exit
        call g (y, toList $ z <| y <| xs)
        inc z
        dec t
        goto a
        ret y
    where n = argv f

rev :: Function -> Function
rev f = function (argv f) $ \(y, xs) -> do
    case xs of
        []     -> call f (y, [])
        (x:xs) -> call f (y, xs ++ [x])
    ret y

(%) :: Function -> Int -> Function
(Function _ f) % n = Function n f -- rewrite argv

-- Initial Functions

n :: Function
n = nullary $ \(_, _) -> ret false

s :: Function
s = unary $ \(y, [x]) -> do
    inc x
    ret x

u :: Int -> Int -> Function
u n i = function n $ \(y, xs) -> ret $ xs !! i

k :: Value -> Function
k n = nullary $ \(y, []) -> do
    set y n
    ret y
