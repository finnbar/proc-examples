-- A few primitives for use throughout my experiments.

module Primitives where

import SF

inc :: (Num a, Arrow ar) => ar a a
inc = arr $ \x -> x+1

sum_ :: (Num a, Arrow ar) => ar (a,a) a
sum_ = arr $ \(x,y) -> x+y

pre :: a -> SF a a
pre v0 = SF $ \v -> (v0, pre v)

dup :: Arrow ar => ar a (a,a)
dup = arr $ \x -> (x,x)

even_ :: (Arrow ar, Integral a) => ar a Bool
even_ = arr $ \x -> even x

boolToNum :: (Arrow ar, Num a) => ar Bool a
boolToNum = arr $ \b -> if b then 0 else 1