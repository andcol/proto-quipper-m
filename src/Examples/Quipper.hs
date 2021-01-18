-------------------------------------------------------------------------
-- In this module I attempt to recreate some of the example programs   --
-- that are presented in a typical Quipper tutorial.                   --
-- Most of the examples come from                                      --
-- "An Introduction to Quantum Programming in Quipper"                 --
-- by A.S. Green et al., 2013                                          --
-------------------------------------------------------------------------


module Examples.Quipper where

import Prelude hiding ((^))

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

share :: Deep '[ '(1,Qubit) ] (Qubit ⊸ (Qubit ⊗ Qubit))
share = λ $ \a -> apply
            (circuit (VLabel 0 `VPair` VLabel 1) (fromGate (C X)) (VLabel 2 `VPair` VLabel 3))
            (a ⊗ label (l1))

bell00 :: Deep '[ '(0,Qubit),'(1,Qubit) ] (Qubit ⊗ Qubit)
bell00 = share ^ (plusMinus False)

--Quantum teleportation

alice :: Deep '[] (Qubit ⊸ Qubit ⊸ (Qubit ⊗ Qubit)) --for now we do not have bits
-- alice = λq. λa. let ⟨q',a'⟩ = apply((⟨0,1⟩,CX,⟨2,3⟩),⟨q,a⟩) in
--                   apply((⟨0,1⟩,Meas2,⟨2,3⟩),⟨apply((0,H,1),q'),a'⟩)
alice = λ $ \q -> λ $ \a ->
        apply (circuit (VLabel 0 `VPair` VLabel 1) (fromGate $ C X) (VLabel 2 `VPair` VLabel 3)) (q ⊗ a)
        `letPair` \(q',a') -> apply (circuit (VLabel 0 `VPair` VLabel 1) (measure 2) (VLabel 2 `VPair` VLabel 3))
        (apply (circuit (VLabel 0) (hadamard 1) (VLabel 1)) q' ⊗ a')


bob :: Deep '[] (Qubit ⊸ (Qubit ⊗ Qubit) ⊸ (Qubit ⊗ (Qubit ⊗ Qubit)))
-- bob = λb. λxy. let ⟨x,y⟩ = xy in
--            let ⟨y',b'⟩ = apply ((⟨0,1⟩,CX,⟨2,3⟩),⟨y,b⟩) in
--            let ⟨x',b''⟩ = apply ((⟨0,1⟩,CX,⟨2,3⟩),⟨x,b'⟩) in
--              ⟨b'',⟨x',y'⟩⟩
bob = λ $ \b -> λ $ \xy ->
      xy `letPair` \(x,y) -> (apply (circuit (VLabel 0 `VPair` VLabel 1) (fromGate $ C X) (VLabel 2 `VPair` VLabel 3)) (y ⊗ b))
      `letPair` \(y',b') -> (apply (circuit (VLabel 0 `VPair` VLabel 1) (fromGate $ C Z) (VLabel 2 `VPair` VLabel 3)) (x ⊗ b'))
      `letPair` \(x',b'') -> b'' ⊗ (x' ⊗ y')

teleport :: Deep '[ '(0,Qubit),'(1,Qubit) ] (Qubit ⊸ (Qubit ⊗ (Qubit ⊗ Qubit)))
-- teleport = λq. let ⟨a,b⟩ = bell00 in
--                let ⟨x,y⟩ = alice q a in
--                  bob b ⟨x,y⟩                 
teleport = λ $ \q ->
            bell00 `letPair` \(a,b) -> alice ^ q ^ a
            `letPair` \(x,y) -> bob ^ b ^ (x ⊗ y)


generate :: KnownCtx γ => Deep γ t -> Circuit --Evaluates an expression and runs the resulting circuit-building computation
generate (exp :: Deep γ t) = c
  where
    res = eval exp (exp2ECtx exp)
    (_,c) = runState res (identity $ exp2LabelContext exp)

test :: IO ()
test = print $ generate (teleport ^ (label l2))
