{-# LANGUAGE Arrows #-}
module TFHaskell.Circuits where

import TFHaskell.BitComputation
import Data.Bits
import Control.Arrow
import Data.Maybe (fromJust)
import Data.List (uncons)
import Data.Int (Int16)

int16ToBitArrayRec :: Int16 -> Int -> [Int]
int16ToBitArrayRec _ (-1) = []
int16ToBitArrayRec x idx = (if testBit x idx then 1 else 0) : int16ToBitArrayRec x (idx - 1)

int16ToBitArray :: Int16 -> [Int]
int16ToBitArray b = int16ToBitArrayRec b ((fromJust.bitSizeMaybe $ b) - 1)

int16FromBitArrayRec :: [Int] -> Int -> Int16 -> Int16
int16FromBitArrayRec [] _ a = a
int16FromBitArrayRec (b:bs) idx a = 
    let r = int16FromBitArrayRec bs (idx - 1) a in
        if b == 1 then setBit r idx else clearBit r idx
    

int16FromBitArray :: [Int] -> Int16
int16FromBitArray bs = int16FromBitArrayRec bs 15 0

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
muxPow2to1 1 = proc (inputs, sels) -> do
    i1 <- bind -< head inputs
    i2 <- bind -< inputs !! 1
    sel <- bind -< head sels

    mux <- mux2to1 -< (i1, i2, sel)

    returnA -< mux

muxPow2to1 n = proc (inputs, sels) -> do
    x1 <- bind -< take (2^(n - 1)) inputs
    x2 <- bind -< drop (2^(n - 1)) inputs
    (sel, rsels) <- bind -< fromJust $ uncons sels

    prev1 <- muxPow2to1 (n - 1) -< (x1, rsels)
    prev2 <- muxPow2to1 (n - 1) -< (x2, rsels)

    prev1sel <- second bnot >>> band -< (prev1, sel)
    prev2sel <- band -< (prev2, sel)

    returnA -< prev1sel .|. prev2sel

nBitMux :: Bits a => Int -> Int -> BitComputation ([[a]], [a]) [a]
nBitMux 0 _ = proc _ -> do returnA -< []
nBitMux n p = proc (inputss, sels) -> do
    (is, ris) <- bind -< (map head inputss, map tail inputss)

    rs <- nBitMux (n - 1) p -< (ris, sels)
    r <- muxPow2to1 p -< (is, sels)

    returnA -< r:rs
