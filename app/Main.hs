{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Program
import Function
import Primitive

-- Primitive recursive functions

add' = rec (u 1 0) (con s [u 3 1])

main :: IO ()
main = print "Hello"
