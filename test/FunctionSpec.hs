module FunctionSpec where

import Test.Hspec
import Test.QuickCheck
import Program
import Function

spec :: Spec
spec = do
    describe "FunctionSpec.id'" $ do
        it "returns the inputs" $
            invoke identity [1000] `shouldBe` (1000 :: Value)

        it "still returns the inputs" $
            id' 1000 `shouldBe` (1000 :: Value)

        it "returns the same element of an *arbitrary* list" $
            property $ \x xs -> all (\x -> id' x == x) $ abs <$> (x:xs)

    describe "FunctionSpec.add'" $ do
        it "returns 7" $
            5 >+< 2 `shouldBe` (7 :: Value)

        it "returns the sum of *arbitrary* 2 numbers" $
            property $ \x xs -> null xs || let x' = abs x; x'' = abs $ head xs
                                           in (x' >+< x'') == (x' + x'')

        it "triples the input of *arbitrary* number" $
            property $ \x xs -> all (\x -> invoke triple [x] == 3 * x) $ abs <$> (x:xs)

-- test functions

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
