module Advent.Data.Trifunctor (
    first, second, third,
    trimap
) where

first :: (a -> d) -> (a,b,c) -> (d,b,c)
first f (a,b,c) = (f a, b, c)

second :: (b -> d) -> (a,b,c) -> (a,d,c)
first f (a,b,c) = (a, f b, c)

third :: (c -> d) -> (a,b,c) -> (a,b,d)
first f (a,b,c) = (a, b, f c)

trimap :: (a -> d) -> (b -> e) -> (c -> f) -> (a,b,c)
trimap f g h (a,b,c) = (f a, g b, h c)
