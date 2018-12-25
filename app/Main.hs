{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Program
import Function
import Primitive

-- Primitive recursive functions

add' = rec (u 1 0) (con s [u 3 1])

identity = unary $ \(y, [x]) -> do
    -- declearation
    [z]    <- freeVars 1
    [a, b] <- freeLabels 2
    -- main program
    _label_ a
    gnz x b
    ret y
    _label_ b
    dec x
    inc y
    gnz x a
    -- return statement
    ret y

id' x = invoke identity [x]

main :: IO ()
main = print "Hello"
