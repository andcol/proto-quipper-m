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


plusMinus :: Bool -> (Circuit, Deep '[ '(0,Qubit) ] Qubit)
plusMinus b = let exp = apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) input in (identity [(0,Qubit)], exp) where
    input = case b of
        True -> (apply (circuit (VLabel 0) (fromGate X) (VLabel 1))) (label l0)
        False -> label l0

plusMinus' :: Bool -> (Circuit, State Circuit (LVal Deep Qubit))
plusMinus' b = let  exp = apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) input
                    res = eval exp (exp2ECtx exp)
                    in (identity (exp2LabelContext exp), res) where
    input = case b of
        True -> (apply (circuit (VLabel 0) (fromGate X) (VLabel 1))) (label l0)
        False -> label l0

test :: IO ()
test = do
    let (circ, exp) = plusMinus False
    let res = eval exp (exp2ECtx exp)
    let (v,c) = runState res circ
    print c

test' :: IO ()
test' = do
    let (circ, v) = plusMinus' True
    print $ snd $ runState v circ
