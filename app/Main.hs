{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Program
import Function

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

add = binary $ \(y, [x1, x2]) -> do
    [z] <- freeVars 1
    [a, b] <- freeLabels 2
    mov y x1
    mov z x2
    _label_ b
    gnz z a
    ret y
    _label_ a
    dec z
    inc y
    goto b
    ret y

(>+<) x1 x2 = invoke add [x1, x2]

triple = unary $ \(y, [x]) -> do
    call add (y, [x, x])
    call add (y, [y, x])
    ret y

main :: IO ()
main = print "Hello"
