module TFHaskell.BitExpressionTree where

import Data.Bits

data BitExprTree = 
    BECons Bool |
    BEVar String |
    BEAnd BitExprTree BitExprTree |
    BEOr BitExprTree BitExprTree |
    BEXor BitExprTree BitExprTree |
    BENot BitExprTree 
    deriving (Eq, Show)

instance Bits BitExprTree where
    (.&.) = BEAnd
    (.|.) = BEOr
    xor = BEXor
    complement = BENot
    shift x _ = x
    rotate x _ = x
    bitSizeMaybe _ = Just 1
    isSigned _ = False

    testBit _ _ = False

    bit 0 = BECons True
    bit _ = BECons False
    
    popCount (BECons True) = 1
    popCount _ = 0


