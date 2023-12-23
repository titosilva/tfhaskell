{-# LANGUAGE Arrows #-}
module TFHaskellTests where
    import Test.Hspec
    import TFHaskell.BitExpressionTree
    import Data.Bits
    import Control.Arrow
    import TFHaskell.BitComputation
    import TFHaskell.Circuits
    import Test.Hspec (SpecWith, describe)
    import Data.Int
    import Data.List (repeat)
    import TFHaskell.Circuits (int16ToBitArray)
    
    andcons :: (Arrow a, Bits c) => c -> a c c
    andcons x = arr (x .&.)

    orcons :: (Arrow a, Bits c) => c -> a c c
    orcons x = arr (x .|.)

    nots :: Bits a => BitComputation a a
    nots = arr complement

    testToBitArray :: SpecWith ()
    testToBitArray = do
        describe "toBitArray" $ do
            it "2 as Int16 to bit array" $ do
                int16ToBitArray (2::Int16) `shouldBe` (replicate 14 0 ++ [1, 0])
            it "5 as Int16 to bit array" $ do
                int16ToBitArray (5::Int16) `shouldBe` (replicate 13 0 ++ [1, 0, 1])
            it "5 as Int16 to bit array then back" $ do
                int16FromBitArray (int16ToBitArray 5) `shouldBe` (5::Int16)

    testBitComputation :: SpecWith ()
    testBitComputation = do
        describe "BitComputation" $ do
            it "1 & 2 = 0" $ do
                runComputation (BitComputation (.&. 1)) 2 `shouldBe` (0::Int)
            it "1 & 1 = 1" $ do
                runComputation (BitComputation (.&. 1)) 1 `shouldBe` (1::Int)
            it "(1 .&. 1) .|. 0 = 1" $ do
                runComputation (andcons 1 >>> orcons 0) 1 `shouldBe` (1::Int)
            it "(1 .&. 0) .|. 0 = 0" $ do
                runComputation (andcons False >>> orcons False) True `shouldBe` False
            it "not 1 = 0" $ do
                runComputation nots True `shouldBe` False
            it "not 0 = 1" $ do
                runComputation nots False `shouldBe` True
            it "1 & 0 = 0" $ do
                runComputation band (True, False) `shouldBe` False
            it "1 & 0 = 0" $ do
                runComputation band (True, True) `shouldBe` True
            it "1 | 0 = 1" $ do
                runComputation bor (True, False) `shouldBe` True
            it "0 | 0 = 0" $ do
                runComputation bor (False, False) `shouldBe` False
            it "0 + 0 + 0 = 0 (no carry)" $ do
                runComputation oneBitAdder (0, 0, 0) `shouldBe` (0, 0::Int)
            it "1 + 0 + 0 = 1 (no carry)" $ do
                runComputation oneBitAdder (1, 0, 0) `shouldBe` (1, 0::Int)
            it "1 + 1 + 0 = 0 (carry)" $ do
                runComputation oneBitAdder (1, 1, 0) `shouldBe` (0, 1::Int)
            it "1 + 1 + 1 = 1 (carry)" $ do
                runComputation oneBitAdder (1, 1, 1) `shouldBe` (1, 1::Int)
            it "1101 + 0010 = 1111 (no carry)" $ do
                runComputation (nBitAdder 4) ([1, 1, 0, 1], [0, 0, 1, 0]) `shouldBe` ([1, 1, 1, 1], 0::Int)
            it "1010 + 1010 = 0100 (carry)" $ do
                runComputation (nBitAdder 4) ([1, 0, 1, 0], [1, 0, 1, 0]) `shouldBe` ([0, 1, 0, 0], 1::Int)
            it "mux 1 0, sel: 0 = 1" $ do
                runComputation mux2to1 (1, 0, 0) `shouldBe` (1::Int)
            it "mux 1 0, sel: 1 = 0" $ do
                runComputation mux2to1 (1, 0, 1) `shouldBe` (0::Int)
            it "mux2to1 1 0, sel: 0 = 1" $ do
                runComputation (muxPow2to1 1) ([1, 0], [0]) `shouldBe` (1::Int)
            it "mux2to1 1 0, sel: 1 = 0" $ do
                runComputation (muxPow2to1 1) ([1, 0], [1]) `shouldBe` (0::Int)
            it "mux4to1 1 0 0 0, sel: 0 0 = 1" $ do
                runComputation (muxPow2to1 2) ([1, 0, 0, 0], [0, 0]) `shouldBe` (1::Int)
            it "mux4to1 1 0 0 0, sel: 1 0 = 0" $ do
                runComputation (muxPow2to1 2) ([1, 0, 0, 0], [1, 0]) `shouldBe` (0::Int)
            it "mux4to1 0 1 0 1, sel: 1 1 = 1" $ do
                runComputation (muxPow2to1 2) ([0, 1, 0, 1], [1, 1]) `shouldBe` (1::Int)
            it "mux4to1 0 1 0 1, sel: 0 1 = 1" $ do
                runComputation (muxPow2to1 2) ([0, 1, 0, 1], [0, 1]) `shouldBe` (1::Int)
            it "mux4to1 0 1 0 1, sel: 0 0 = 0" $ do
                runComputation (muxPow2to1 2) ([0, 1, 0, 1], [0, 0]) `shouldBe` (0::Int)
            it "16 bit mux with 4 inputs, sel = 00" $ do
                runComputation (nBitMux 16 2) (map int16ToBitArray [2, 231, 22, -1], [0, 0]) `shouldBe` int16ToBitArray (2::Int16)
            it "16 bit mux with 4 inputs, sel = 11" $ do
                runComputation (nBitMux 16 2) (map int16ToBitArray [2, 231, 22, -1], [1, 1]) `shouldBe` int16ToBitArray (-1::Int16)
            it "16 bit mux with 4 inputs, sel = 01" $ do
                runComputation (nBitMux 16 2) (map int16ToBitArray [2, 231, 22, -1], [0, 1]) `shouldBe` int16ToBitArray (231::Int16)
            it "16 bit mux with 4 inputs, sel = 10" $ do
                runComputation (nBitMux 16 2) (map int16ToBitArray [2, 231, 22, -1], [1, 0]) `shouldBe` int16ToBitArray (22::Int16)

    testBitExprTree :: SpecWith ()
    testBitExprTree = do
        describe "BitExpressionTree" $ do
            it "Expression tree And" $ do
                BECons False .&. BECons True `shouldBe` BEAnd (BECons False) (BECons True)
            it "Expression tree Or" $ do
                BECons False .|. BECons True `shouldBe` BEOr (BECons False) (BECons True)
            it "Expression tree Not" $ do
                complement (BECons True) `shouldBe` BENot (BECons True)
            it "Expression tree andcons" $ do
                runComputation (andcons (BECons True)) (BECons False) `shouldBe` BEAnd (BECons True) (BECons False)
            it "Expression tree mux2to1" $ do
                runComputation mux2to1 (BEVar 1, BEVar 2, BEVar 3) `shouldBe` BEOr (BEAnd (BEVar 1) (BENot (BEVar 3))) (BEAnd (BEVar 2) (BEVar 3))
