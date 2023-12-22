module TFHaskell.EncryptedComputation where

import TFHaskell.BitComputation
import TFHaskell.BitExpressionTree
import TFHEBindings.TFHE
import Data.Bits
import System.IO.Unsafe

compile :: BitComputation BitExprTree BitExprTree -> BitExprTree
compile c = runComputation c (BEVar "n0")

{-# NOINLINE run_expression_encrypted #-} 
run_expression_encrypted :: BitExprTree -> TFHEPtr -> TFHEPtr -> IO TFHEPtr
run_expression_encrypted (BEVar _) pub x = return x
run_expression_encrypted (BECons b) pub x = encrypted_constant pub (if b then 1 else 0)
run_expression_encrypted (BEAnd x y) pub r = do
    xe <- run_expression_encrypted x pub r
    ye <- run_expression_encrypted y pub r
    encrypted_and pub xe ye
run_expression_encrypted (BEOr x y) pub r = do
    xe <- run_expression_encrypted x pub r
    ye <- run_expression_encrypted y pub r
    encrypted_or pub xe ye    
run_expression_encrypted (BEXor x y) pub r = do
    xe <- run_expression_encrypted x pub r
    ye <- run_expression_encrypted y pub r
    encrypted_xor pub xe ye

run_expression_encrypted (BENot x) pub r = do
    xe <- run_expression_encrypted x pub r
    encrypted_not pub xe



