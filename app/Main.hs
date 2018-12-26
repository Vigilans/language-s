{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Program
import Function
import Primitive

-- Primitive recursive functions

id' = u 1 0

add = rec id' (con s [u 3 1])
mul = rec (n % 1) (con add [u 3 1, u 3 2])
fac = rec (con s [n]) (con mul [con s [u 2 0], u 2 1])
pow = rec (con s [n] % 1) (con mul [u 3 1, u 3 2])
pre = rec n (u 2 0)
sub = rec (u 1 0) (con pre [u 3 1])

abf = con add [sub, rev sub]
inv = con sub [con s [n] % 1, id']
sgn = con inv [inv]

or = con sgn [add]
and = con sgn [mul]

eq = con inv [abf]
le = con inv [sub]
ge = con rev [le]
lt = con inv [ge]
gt = con inv [le]

main :: IO ()
main = print "Hello"
