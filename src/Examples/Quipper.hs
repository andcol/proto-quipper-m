-------------------------------------------------------------------------
-- In this module I attempt to recreate some of the example programs   --
-- that are presented in a typical Quipper tutorial.                   --
-- Most of the examples come from                                      --
-- "An Introduction to Quantum Programming in Quipper"                 --
-- by A.S. Green et al., 2013                                          --
-------------------------------------------------------------------------

module Examples.Quipper where

import LNLHask
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive
import Language.Core
import Language.Lift
import Language.Box

import Control.Monad.State.Lazy hiding (lift)

import Data.Proxy
import Language.LabelContexts
import GHC.TypeLits


plusMinus :: Bool -> Deep '[ '(0,Qubit) ] Qubit
plusMinus b = apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) input where
    input = case b of
        True -> (apply (circuit (VLabel 0) (fromGate X) (VLabel 1))) (label l0)
        False -> label l0

share :: (KnownNat x, KnownNat (x+1),
    CMerge '[ '(x,Qubit) ] '[ '(x+1,Qubit) ] '[ '(x,Qubit), '(x+1,Qubit) ])
    => Deep '[ '(x,Qubit) ] Qubit -> Deep '[ '(x,Qubit), '(x+1,Qubit) ] (Qubit ⊗ Qubit) --TODO this should be more general!
share (exp :: Deep '[ '(x,Qubit) ] Qubit) = apply
                                            (circuit (VLabel 0 `VPair` VLabel 1) (fromGate (C X)) (VLabel 2 `VPair` VLabel 3))
                                            (exp ⊗ label (Proxy :: Proxy (x+1)))

bell00 :: Deep '[ '(0,Qubit),'(1,Qubit) ] (Qubit ⊗ Qubit)
bell00 = let a = plusMinus False in share a




generate :: KnownCtx γ => Deep γ t -> Circuit --Evaluates an expression and runs the resulting circuit-building computation
generate (exp :: Deep γ t) = c
  where
    res = eval exp (exp2ECtx exp)
    (_,c) = runState res (identity $ exp2LabelContext exp)

test :: IO ()
test = print $ generate bell00
