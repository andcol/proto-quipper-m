module Language.Core where

import Types
import Classes
import Circuit

data QubitSig exp = MkQubit
type Qubit = MkLType MkQubit

data CircSig exp = MkCirc exp exp
type Circ t u = MkLType ('MkCirc t u)

class HasCore (exp :: Sig) where
    labelled :: LabelledCircuit c => exp γ1 Qubit -> c -> exp γ2 Qubit -> exp '[] (Circ Qubit Qubit) --missing check to see whether c is actually Qubit -> Qubit
    apply :: forall (γ1 :: Ctx) (γ2 :: Ctx) (γ :: Ctx).
                CMerge γ1 γ2 γ => exp γ1 (Circ Qubit Qubit) -> exp γ2 Qubit -> exp γ Qubit