{-# LANGUAGE Arrows #-} 
module TFHaskellTests where
    import Test.Hspec
    import TFHaskell.BitComputation
    import TFHaskell.BitExpressionTree
    import Data.Bits
    import Control.Arrow

    andcons :: (Arrow a, Bits c) => c -> a c c
    andcons x = arr (x .&.)
    
    orcons :: (Arrow a, Bits c) => c -> a c c
    orcons x = arr (x .|.)
    
    nots :: Bits a => BitComputation a a
    nots = arr complement

    mux2to1 :: (Bits a) => a -> a -> BitComputation a a
    mux2to1 x y = proc z -> do
        a <- andcons x -< z
        b <- andcons y <<< nots -< z
        returnA -< (a .|. b)

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
            it "Multiplex 2 to 1 v1" $ do
                runComputation (mux2to1 True False) True `shouldBe` True
            it "Multiplex 2 to 1 v2" $ do
                runComputation (mux2to1 True False) False `shouldBe` False
            it "Multiplex 2 to 1 v2" $ do
                runComputation (mux2to1 False True) False `shouldBe` True

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
                runComputation (mux2to1 (BEVar "x") (BEVar "y")) (BEVar "z") `shouldBe` BEOr (BEAnd (BEVar "x") (BEVar "z")) (BEAnd (BEVar "y") (BENot (BEVar "z")))
