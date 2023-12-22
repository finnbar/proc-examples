-- A collection of tests on if/case, which desugar to use of Either values and the ||| combinator.

{-# LANGUAGE Arrows #-}
module Choice where

import Primitives

import Control.Arrow

-- A deliberately erroneous pair of examples with type mismatches on the branches.
erroneousIf :: ArrowChoice ar => ar Int Int
erroneousIf = proc x -> do
    ev <- even_ -< x
    if ev then inc -< x else even_ -< x

erroneousCase :: ArrowChoice ar => ar Int Int
erroneousCase = proc x -> do
    case x of
        1 -> even_ -< x
        2 -> inc -< x
        n -> sum_ -< (x,x)

-- An example of nested case, which should produce nested Either values in the desugaring.
nestedCase :: ArrowChoice ar => ar (Int, Int) Int
nestedCase = proc (x,y) -> do
    case x of
        1 -> do
            z <- sum_ -< (x,y)
            case z of
                1 -> inc -< y
                n -> sum_ -< (x, n)
        2 -> do
            z <- inc -< y
            case z of
                1 -> sum_ -< (y, y)
                n -> returnA -< n
        n -> do
            ev <- even_ -< y
            if ev then returnA -< y else returnA -< n

-- This program is a mess, but tests the following on an input (x,y):
-- if x=1 then if y=0 then 1 else 2+y
-- if x=2 then if y=0 then 0 else 1+y
-- else if y is even then y else x