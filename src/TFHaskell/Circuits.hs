{-# LANGUAGE Arrows #-}
module TFHaskell.Circuits where

import TFHaskell.BitComputation
import Data.Bits
import Control.Arrow
import GHC.Natural
import Data.Maybe (fromJust)
import Data.List (uncons)

bind :: BitComputation a a
bind = proc a -> do
    returnA -< a

bnot :: Bits a => BitComputation a a
bnot = proc a -> do
    notA <- arr complement -< a
    returnA -< notA

band :: Bits a => BitComputation (a, a) a
band = proc (a, b) -> do
    returnA -< a .&. b

bor :: Bits a => BitComputation (a, a) a
bor = proc (a, b) -> do
    returnA -< a .|. b

bxor :: Bits a => BitComputation (a, a) a
bxor = proc (a, b) -> do
    returnA -< xor a b

oneBitAdder :: Bits a => BitComputation (a, a, a) (a, a)
oneBitAdder = proc (a, b, cin) -> do
    axorb <- bxor -< (a, b)
    aandb <- band -< (a, b)
    candxor <- band -< (axorb, cin)

    s <- bxor -< (cin, axorb)
    cout <- bor -< (candxor, aandb)

    returnA -< (s, cout)

nBitAdder :: Bits a => Int -> BitComputation ([a], [a]) ([a] , a)
nBitAdder 0 = proc (_, _) -> do
    returnA -< ([], zeroBits)
nBitAdder 1 = proc (x, y) -> do
    a <- bind -< head x
    b <- bind -< head y

    (s, cout) <- oneBitAdder -< (a, b, zeroBits)
    returnA -< ([s], cout)

nBitAdder n = proc (x, y) -> do
    (a, xs) <- bind -< fromJust $ uncons x
    (b, ys) <- bind -< fromJust $ uncons y

    (ss, prevCout) <- nBitAdder (n - 1) -< (xs, ys)

    (s, cout) <- oneBitAdder -< (a, b, prevCout)

    returnA -< (s:ss, cout)

mux2to1 :: Bits a => BitComputation (a, a, a) a
mux2to1 = proc (i1, i2, sel) -> do
    i1sel <- second bnot >>> band -< (i1, sel)
    i2sel <- band -< (i2, sel)

    returnA -< i1sel .|. i2sel

muxPow2to1 :: Bits a => Int -> BitComputation ([a], [a]) a
muxPow2to1 0 = proc (x, _) -> do returnA -< head x
muxPow2to1 1 = proc (x, y) -> do
    i1 <- bind -< head x
    i2 <- bind -< x !! 1
    sel <- bind -< head y

    mux <- mux2to1 -< (i1, i2, sel)

    returnA -< mux

muxPow2to1 n = proc (x, y) -> do
    x1 <- bind -< take (2^(n - 1)) x
    x2 <- bind -< drop (2^(n - 1)) x
    (sel, ys) <- bind -< fromJust $ uncons y

    prev1 <- muxPow2to1 (n - 1) -< (x1, ys)
    prev2 <- muxPow2to1 (n - 1) -< (x2, ys)

    prev1sel <- second bnot >>> band -< (prev1, sel)
    prev2sel <- band -< (prev2, sel)

    returnA -< prev1sel .|. prev2sel