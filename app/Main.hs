{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Program
import Function
import Primitive

-- Primitive recursive functions

pre = rec z (u 2 0)
add = rec (u 1 0) (con s [u 3 1])
mul = rec (z % 1) (con add [u 3 1, u 3 2])
fac = rec (k 1) (con mul [con s [u 2 0], u 2 1])
pow = rev $ rec (k 1 % 1) (con mul [u 3 1, u 3 2])
sub = rev $ rec (u 1 0) (con pre [u 3 1])
abf = con add [sub, rev sub]

inv = con sub [k 1 % 1, u 1 0]
sgn = con inv [inv]
or'  = con sgn [add]
and' = con sgn [mul]

eq = con inv [abf]
le = con inv [sub]
ge = rev le
lt = con inv [ge]
gt = con inv [le]

if' p t f = con add [con mul [p, t], con mul [con inv [p], f]]

sum' = fold add
prod = fold mul
any' f = con sgn [sum' f]
all' f = con sgn [prod f]

min' p = if' (any' p) (sum' $ prod $ con inv [p]) (z % argc p)
div' = con (min' $ con gt [con mul [con s [u 3 0], u 3 1], u 3 2]) [u 2 0, u 2 1, u 2 0] 
mod' = con sub [u 2 0, con mul [u 2 1, div']]

-- case' m ps ts f | length ps == length ts = 
--     rec (if' (head ps) (head ts) f)

main :: IO ()
main = print "Hello"
