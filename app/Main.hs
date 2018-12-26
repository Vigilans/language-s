{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Program
import Function
import Primitive

-- Primitive recursive functions

add = rec (u 1 0) (con s [u 3 1])
mul = rec n (con add [u 3 1, u 3 2])
fac = rec (k 1) (con mul [con s [u 2 0], u 2 1])
pow = rec (con s [n]) (con mul [u 3 1, u 3 2])
pre = rec (k 0) (u 2 0)
sub = rec (u 1 0) (con pre [u 3 1])

main :: IO ()
main = print "Hello"
