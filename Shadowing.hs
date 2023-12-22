-- Trying some name shadowing.

{-# LANGUAGE Arrows, ScopedTypeVariables #-}
module Shadowing where

import Primitives

import Control.Arrow

-- Testing nested proc statements with name shadowing.
-- Also turns out this tests -<<! (I am not very experienced with -<< so didn't intend this!)
nestedProc :: forall ar. (Arrow ar, ArrowApply ar) => ar Int Int
nestedProc = proc x -> do
    y <- inc -< x
    z <- sum_ -< (x, y)
    let proc' :: ar Int Int
        proc' = proc z' -> do
            -- This y refers to the y in the outer proc.
            z <- inc -< y
            -- This should refer to the z defined on the previous line.
            sum_ -< (z', z)
    -- This z refers to the older z, so the sum of x and y.
    proc' -<< z+1

-- This program, given x:
-- Sets the outer z to x+y+1 = x+(x+1)+1, then runs proc' with it to get:
-- z'+z = z'+(y+1) = (x+x+1+1)+(x+1+1) = 3x+4.

-- Testing name shadowing within a single command.
-- We deliberately give the redefinition a new type to make sure that we cannot use the old.
shadowingProc :: Arrow ar => ar Int Int
shadowingProc = proc x -> do
    -- If you want an erroneous program, swap these two lines.
    y <- inc -< x
    y <- even_ -< x
    -- This should refer to the latter y.
    z <- boolToNum -< y
    returnA -< z