module Language.Core where

import Types
import Classes
import Interface
import Circuit.Dynamic.Class
import DeepEmbedding

import Control.Monad.Identity

data QubitSig exp = MkQubit
type Qubit = MkLType MkQubit

data CircSig exp = MkCirc exp exp
type Circ t u = MkLType ('MkCirc t u)

class Match langSig circSig
instance Match Qubit '[Qubit]
--TODO Match for circuits with more than one input / classical input

class HasTensor exp => HasCore exp where
    label :: Label -> exp γ Qubit --not sure γ is correct: should unify when eval is called??
    circuit :: LabelledCircuit circ
                => exp γ1 Qubit -> circ -> exp γ2 Qubit -> exp '[] (Circ Qubit Qubit)
    apply :: forall (γ1 :: Ctx) (γ2 :: Ctx) (γ :: Ctx).
                CMerge γ1 γ2 γ => exp γ1 (Circ Qubit Qubit) -> exp γ2 Qubit -> exp γ Qubit

--DEEP EMBEDDING

type instance Effect _ = Identity

data CoreExp :: Sig where
    Label :: Label -> CoreExp γ Qubit
    Circuit :: LabelledCircuit circ
                => Deep γ1 Qubit -> circ -> Deep γ2 Qubit -> CoreExp '[] (Circ Qubit Qubit)
    Apply :: forall (γ1 :: Ctx) (γ2 :: Ctx) (γ :: Ctx).
                CMerge γ1 γ2 γ => Deep γ1 (Circ Qubit Qubit) -> Deep γ2 Qubit -> CoreExp γ Qubit

instance HasCore Deep where
    label = Dom . Label
    circuit l c l' = Dom $ Circuit l c l'
    apply c l = Dom $ Apply c l

data instance LVal Deep Qubit = VLabel Label
data instance LVal Deep (Circ t u)  where
    VCirc :: LabelledCircuit circ
                => Deep γ1 t -> circ -> Deep γ2 u -> LVal Deep (Circ t u)

instance Domain CoreExp where
    evalDomain (Label id) ρ = return $ VLabel id
    evalDomain (Circuit l c l') ρ = return $ VCirc l c l'
    evalDomain (Apply (d :: Deep γ1 (Circ Qubit Qubit)) (k :: Deep γ2 Qubit)) ρ = do
        VCirc l c l' <- eval c ρ1
        VLabel id <- eval k ρ2
        --append to be added to get c' and k'
        return $ VLabel 0 --should be k'
        where
            (ρ1,ρ2) = splitECtx @γ1 @γ2 ρ 
        