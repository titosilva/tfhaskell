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

band :: (Bits a) => BitComputation (a, a) a
band = proc (a, b) -> do
    returnA -< a .&. b

bor :: (Bits a) => BitComputation (a, a) a
bor = proc (a, b) -> do
    returnA -< a .|. b

bxor :: (Bits a) => BitComputation (a, a) a
bxor = proc (a, b) -> do
    returnA -< xor a b

oneBitAdder :: (Bits a) => BitComputation (a, a, a) (a, a)
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