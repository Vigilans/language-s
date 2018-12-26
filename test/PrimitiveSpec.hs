module PrimitiveSpec where

import Test.Hspec
import Test.QuickCheck
import Program
import Function
import Primitive

spec :: Spec
spec =
    describe "PrimitiveSpec.intials'" $ do
        it "z() = 0" $
            property $ \x xs -> all (\x -> invoke z [x] == 0) $ abs <$> (x:xs)

        it "s(x) = x + 1" $
            property $ \x xs -> all (\x -> invoke s [x] == (x + 1)) $ abs <$> (x:xs)

        it "u(1, 0)(x) = x" $
            property $ \x xs -> all (\x -> invoke (u 1 0) [x] == x) $ abs <$> (x:xs)

        it "u(3, 1)(x1, x2, x3) = x2" $
            property $ \x1 xs -> length xs < 2 || let x2 = head xs; x3 = xs !! 1 in invoke (u 3 1) [x1, x2, x3] == x2
