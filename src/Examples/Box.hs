module Examples.Box where

import LNLHask
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Control.Monad.State.Lazy hiding (lift)

import Language.Core
import Language.Lift
import Language.Box

instance (Show (LVal Deep l), Show (LVal Deep r)) => Show (LVal Deep (l ⊗ r)) where
    show (VPair x y) = "⟨" ++ (show x) ++ ", " ++ (show y) ++ "⟩"

instance (Show (LVal Deep i), Show (LVal Deep o)) => Show (LVal Deep (Circ i o)) where
    show (VCirc l d l') = "(" ++ (show l) ++ ", " ++ (show d) ++ ", " ++ (show l') ++ ")"

test1 :: IO () --tests single-qubit gate application and boxing
test1 = do
    let expr = box @Deep $ lift $ λ $ \l -> apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) l
    let res = eval expr eEmpty
    let out = runState res (identity [(0,Qubit)])
    print out

test2 :: IO () --test two-qubit gate application and boxing
test2 = do
    let expr = box @Deep $ lift $ λ $ \l -> apply (circuit ((VLabel 0) `VPair` (VLabel 1)) (hadamard 2) ((VLabel 2) `VPair` (VLabel 3))) l
    let res = eval expr eEmpty
    let (v,s) = runState res (identity [(0,Qubit),(1,Qubit)])
    print v