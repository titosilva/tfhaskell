module Main where
    import Test.Hspec

    import TFHEBindingsTests
    import EncryptedComputationTests
    import TFHaskellTests

    main :: IO ()
    main = hspec $ do
        testToBitArray
        testBitComputation
        testBitExprTree
        testTFHEBindings
        testEncryptedComputation
