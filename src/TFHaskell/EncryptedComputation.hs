module TFHaskell.EncryptedComputation where

import TFHaskell.BitComputation
import TFHaskell.BitExpressionTree
import TFHEBindings.TFHE

compile :: BitComputation BitExprTree BitExprTree -> BitExprTree
compile c = runComputation c (BEVar "n0")

runExpressionEncrypted :: BitExprTree -> TFHEPtr -> TFHEPtr -> IO TFHEPtr
runExpressionEncrypted (BEVar _) _ x = return x

runExpressionEncrypted (BECons b) pub _ = encrypted_constant pub (if b then 1 else 0)

runExpressionEncrypted (BEAnd x y) pub r = do
    xe <- runExpressionEncrypted x pub r
    ye <- runExpressionEncrypted y pub r
    encrypted_and pub xe ye

runExpressionEncrypted (BEOr x y) pub r = do
    xe <- runExpressionEncrypted x pub r
    ye <- runExpressionEncrypted y pub r
    encrypted_or pub xe ye    
    
runExpressionEncrypted (BEXor x y) pub r = do
    xe <- runExpressionEncrypted x pub r
    ye <- runExpressionEncrypted y pub r
    encrypted_xor pub xe ye

runExpressionEncrypted (BENot x) pub r = do
    xe <- runExpressionEncrypted x pub r
    encrypted_not pub xe



