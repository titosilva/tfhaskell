module TFHaskell.BitExpressionTree where

import Data.Bits
import Data.Maybe

data BitExprTree = 
    BECons Bool |
    BEVar String |
    BEAnd BitExprTree BitExprTree |
    BEOr BitExprTree BitExprTree |
    BENot BitExprTree
    deriving (Eq, Show)

instance Bits BitExprTree where
    (.&.) = BEAnd
    (.|.) = BEOr
    complement = BENot
    shift x _ = x
    rotate x _ = x
    bitSizeMaybe _ = Just 1
    isSigned _ = False

    testBit x 0 = False
    testBit x _ = False

    bit 0 = BECons True
    bit _ = BECons False
    
    popCount (BECons True) = 1
    popCount _ = 0


