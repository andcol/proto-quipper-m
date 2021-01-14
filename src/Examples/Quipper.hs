-------------------------------------------------------------------------
-- In this module I attempt to recreate some of the example programs   --
-- that are presented in a typical Quipper tutorial.                   --
-- Most of the examples come from                                      --
-- "An Introduction to Quantum Programming in Quipper"                 --
-- by A.S. Green et al.                                                --
-------------------------------------------------------------------------

module Examples.Quipper where

import Types
import Interface hiding (lift)
import DeepEmbedding
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Control.Monad.State.Lazy hiding (lift)

import Language.Core
import Language.Lift
import Language.Box

import Data.Proxy
import Language.LabelContexts
import GHC.TypeLits


plusMinus :: Bool -> Deep '[ '(0,Qubit) ] Qubit
plusMinus b = apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) input where
    input = case b of
        True -> (apply (circuit (VLabel 0) (fromGate X) (VLabel 1))) (label l0)
        False -> label l0

share :: Deep '[ '(0,Qubit) ] Qubit -> Deep '[ '(0,Qubit), '(1,Qubit) ] (Qubit ⊗ Qubit) --TODO this should be more general!
share exp = apply
            (circuit (VLabel 0 `VPair` VLabel 1) (fromGate (C X)) (VLabel 2 `VPair` VLabel 3))
            (exp ⊗ label l1)

bell00 :: Deep '[ '(0,Qubit),'(1,Qubit) ] (Qubit ⊗ Qubit)
bell00 = let a = plusMinus False in share a

printCirc :: KnownCtx γ => Deep γ t -> IO () --Generate and print a circuit as a string
printCirc (exp :: Deep γ t) = do
    let res = eval exp (exp2ECtx exp)
    let (_,c) = runState res (identity $ exp2LabelContext exp)
    print c

test :: IO ()
test = printCirc $ bell00
