-- The desugaring to loop is not very well documented, so a lot of this is guesswork.
-- Examples of rec within proc. This should desugar to use of loop :: arr (a, c) (b, c), where
-- the looped value (of type c) consists of the environment of all variables defined within that
-- rec, the input a consists of the environment of all variables defined before the rec and the
-- output b consists of the environment of all variables defined up to this point. Note that the
-- variables defined within the rec should be available after that rec - the rec is not its own
-- scope, but just allows for mutually recursive bindings.

-- There's a brief note with each rec about how it is well-formed - that is, we can actually
-- execute it without getting a <loop> from the GHC runtime system. These rely on `pre v`, a delay
-- operator which returns v on first invocation and the previous value given to it during future
-- invocations. Thus, its output is always available since it is the input it received previously.

{-# LANGUAGE Arrows #-}
module ProcRec where

import Primitives
import SF

import Control.Arrow (returnA)

-- We first test a particularly large rec clause.
-- We use SF here as there is no class for arrows which contain pre within Control.Arrow (the
-- arrows package contains ArrowCircuit, but I wanted minimal imports), and pre is an easy way to
-- make sure that our loops (and thus rec commands) are well-formed.
largeRec :: SF Int Int
largeRec = proc x -> do
    -- NOTE This is well-formed: x is available at the start of execution, as is w.
    -- x gives us x', w gives us (p, q), p gives us p''' and thus we can get y.
    -- Therefore we can get z, which gives us the output and w after the delay.
    rec
        x' <- inc -< x
        z <- sum_ -< (x', y)
        w <- pre 0 -< z
        (p, q) <- dup -< w
        -- If you want an erroneous program, you can change p''' to p'' in the sum_ line.
        p' <- inc -< p
        p'' <- even_ -< p'
        p''' <- boolToNum -< p''
        y <- sum_ -< (p''', q)
    returnA -< z

-- This program effectively does the following:
-- Set w = 0.
-- Repeatedly:
--   Set y = w + 1 if w is even, else w
--   Set z = y + x + 1
--   Set w = z
-- Example run: setting x to [1,2,3,4,5] gives z = [3,6,11,16,23].

-- Now we test name shadowing, by redefining y from outside of the rec.
-- We test two similar examples with a minor difference, that I _believe_ should act the same.
-- (They act the same in GHC 9.4.7.)
shadowingRec :: SF Int Int
shadowingRec = proc x -> do
    y <- inc -< x
    rec
        -- This y is newly defined and shadows the old one, leading to a well-formed loop.
        y <- pre 0 -< p
        p <- sum_ -< (x, y)
    returnA -< p

shadowingRec' :: SF Int Int
shadowingRec' = proc x -> do
    y <- inc -< x
    rec
        -- I think this y _should_ refer to the one defined within this rec (on the next line).
        -- I honestly don't know though.
        p <- sum_ -< (x, y)
        y <- pre 0 -< p
    returnA -< p

-- The programs should do the following:
-- Set y = 0.
-- Repeatedly:
--   Set p = x + y
--   Set y = p
-- Example run: setting x to [1,2,3,4,5] gives p = [1,3,6,10,15].
-- (Again, I'm unsure exactly how the scoping rules work here within proc, so I'm going off my
-- intuition and might be wrong in the comments.)

-- Nested rec commands.
nestedRec :: SF Int Int
nestedRec = proc x -> do
    y <- inc -< x
    -- NOTE This loop is well-formed: you have access to y from the input of nestedRec, and z from
    -- the output of pre 0.
    rec
        z' <- sum_ -< (y, p)
        z <- pre 0 -< z'
        -- NOTE This loop is also well-formed: you have access to z from the outer loop, and w from
        -- the output of pre 0. This gives you v.
        rec
            w <- pre 0 -< v
            v <- sum_ -< (w, z)
        
        -- The inner loop gives you v, and the start of this loop gives you z, so you can
        -- determine p.
        p <- sum_ -< (v, z)
    returnA -< p

-- This program effectively does the following:
-- Set z=0, w=0.
-- Repeatedly:
--   Set v=w+z
--   Set w=v
--   Set p=v+z
--   Set z=x+1+p
-- Example run: setting x to [1,2,3] gives p = [0,4,16]
-- v and w = [0,2,9] z = [2,7,20]