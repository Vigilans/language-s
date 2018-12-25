{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Program
import Function

idP = unary $ \(y, [x], exit) -> do
    -- declearation
    [z]    <- freeVars 1
    [a, b] <- freeLabels 2
    -- main program
    _label_ a
    gnz x b
    goto exit
    _label_ b
    dec x
    inc y
    gnz x a
    -- return statement
    return y

id' x = invoke idP [x]

addP = binary $ \(y, [x1, x2], exit) -> do
    [z] <- freeVars 1
    [a, b] <- freeLabels 2
    mov y x1
    mov z x2
    _label_ b
    gnz z a
    goto exit
    _label_ a
    dec z
    inc y
    goto b
    return y

(>+<) x1 x2 = invoke addP [x1, x2]

tripleP = unary $ \(y, [x], exit) -> do
    call addP (y, [x, x])
    call addP (y, [y, x])
    return y

main :: IO ()
main = print "Hello"
