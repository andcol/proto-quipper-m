module Examples.Core where

import LNLHask
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive
import Language.Core

import Control.Monad.State.Lazy


test1 :: IO () --tests single-qubit gate application
test1 = do
    let expr = apply @Deep (circuit (VLabel 0) (fromGate H) (VLabel 1)) (label l0)
    let res = eval expr (addECtx l0 (VLabel 0) eEmpty)
    let out = runState res (identity [(0,Qubit)])
    print out

test2 :: IO () --test two-qubit gate application
test2 = do
    let expr = apply @Deep (circuit ((VLabel 0) `VPair` (VLabel 1)) (hadamard 2) ((VLabel 2) `VPair` (VLabel 3))) ((label l0) ⊗ (label l1))
    let res = eval expr (addECtx l1 (VLabel 1) $ addECtx l0 (VLabel 0) eEmpty)
    let (_,s) = runState res (identity [(0,Qubit),(1,Qubit)])
    print s

test3 :: IO () --like test 2. Circuit produced is equivalent up to a renaming of (output) labels
test3 = do
    let expr = apply @Deep (circuit (VLabel 0) (fromGate H) (VLabel 1)) (label l0) ⊗ apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) (label l1)
    let res = eval expr (addECtx l1 (VLabel 1) $ addECtx l0 (VLabel 0) eEmpty)
    let (_,s) = runState res (identity [(0,Qubit),(1,Qubit)])
    print s

--test4 :: IO () --like test 3, but violates linear constraints (does not compile)
--test4 = do
--    let expr = apply @Deep (circuit (VLabel 0) (fromGate H) (VLabel 1)) (label l0) ⊗ apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) (label l0)
--    let res = eval expr (addECtx l1 (VLabel 1) $ addECtx l0 (VLabel 0) eEmpty)
--    let (_,s) = runState res (identity [(0,Qubit),(1,Qubit)])
--    print s

test5 :: IO () --This shows that abstractions successfully steer clear of capturing labels, so the context injection trick makes sense
test5  = do
    let expr = λ $ \x -> letUnit x (apply @Deep (circuit (VLabel 0) (fromGate H) (VLabel 1)) (label l0))
    let res = eval expr (addECtx l0 (VLabel 0) eEmpty)
    let out = runState res (identity [(0,Qubit)])
    print "ok"