module Language.Core where

import Types
import Classes
import Interface
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive
import DeepEmbedding

import Control.Monad.State.Lazy

type UnderlyingCircuit = Circuit --choose the implementation of the circuit here!

data QubitSig exp = MkQubit
type Qubit = MkLType 'MkQubit

data CircSig exp = MkCirc exp exp
type Circ t u = MkLType ('MkCirc t u)

class MType (exp :: Sig) (t :: LType) where
    labels :: LVal exp t -> [Label] --extracts the labels of a value of MType T
    signature :: LVal exp t -> [WireType] --extracts a runtime representation (WireType) of the MType T of a value
    fromLabelContext :: LabelContext -> exp '[] t --constructs a value of MType T from a label context TODO should not be '[]
    toLabelContext :: LVal exp t -> LabelContext --retrieves the label context Q such that \emptyset;Q |- \vec\ell : T
    toLabelContext vl = zip (labels vl) (signature vl)

class HasTensor exp => HasCore exp where
    label :: Label -> exp '[] Qubit --not sure γ is correct: should unify when eval is called??
    circuit :: (MType exp t, MType exp u) => exp '[] t -> UnderlyingCircuit -> exp '[] u -> exp '[] (Circ t u)
    apply :: (MType exp t, MType exp u, CMerge γ1 γ2 γ) => exp γ1 (Circ t u) -> exp γ2 t -> exp γ u

    --technically the labels in (l,D,l') cannot be ANY term of type Qubit, they should be values, how do I encode that?

--DEEP EMBEDDING

type instance Effect _ = State UnderlyingCircuit 
    

data CoreExp :: Sig where
    Label :: Label -> CoreExp '[] Qubit
    Circuit :: (MType Deep t, MType Deep u) => Deep '[] t -> UnderlyingCircuit -> Deep '[] u -> CoreExp '[] (Circ t u)
    Apply :: (MType Deep t, MType Deep u, CMerge γ1 γ2 γ) => Deep γ1 (Circ t u) -> Deep γ2 t -> CoreExp γ u

instance HasCore Deep where
    label = Dom . Label
    circuit l c l' = Dom $ Circuit l c l'
    apply c l = Dom $ Apply c l

data instance LVal Deep Qubit = VLabel Label
data instance LVal Deep (Circ t u)  where
    VCirc :: (MType Deep t, MType Deep u) => LVal Deep t -> UnderlyingCircuit -> LVal Deep u -> LVal Deep (Circ t u)

instance MType Deep Qubit where
    labels (VLabel id) = [id]
    signature _ = [Qubit]
    fromLabelContext [(l,Qubit)] = Dom $ Label l
instance (MType Deep t) => MType Deep (Qubit ⊗ t) where
    labels (VPair (VLabel id) x) = id : (labels x)
    signature (VPair _ r) = Qubit : (signature r)
    fromLabelContext ((l,Qubit):lcr) = Dom $ Pair (Dom $ Label l) (fromLabelContext lcr)


instance Domain CoreExp where
    evalDomain (Label id) ρ = return $ VLabel id
    evalDomain (Circuit (l :: Deep '[] t) (c :: UnderlyingCircuit) (l' :: Deep '[] u)) eEmpty = do
        vl <- eval l eEmpty
        vl' <- eval l' eEmpty
        if (toLabelContext vl) == (inputsOf c) && (toLabelContext vl') == (outputsOf c) --akin to the compile-time check "c ∈ Mℒ(Q,Q') where Q and Q' type vl and vl' in t and u respectively"
            then return $ VCirc vl c vl'
            else error "mismatch between term-level labels and actual circuit labels"
    evalDomain (Apply (circuit :: Deep γ1 (Circ t u)) (targets :: Deep γ2 t)) ρ = do
        VCirc l d l' <- eval circuit ρ1
        targetVec <- eval targets ρ2
        outputVec <- appendM (labels targetVec) d
        let out = fromLabelContext (zip outputVec (signature l'))
        eval out eEmpty
        where
            (ρ1,ρ2) = splitECtx @γ1 @γ2 ρ

--TESTS

test1 :: IO () --tests single-qubit gate application
test1 = do
    let expr = apply @Deep (circuit (label 0) (fromGate H) (label 1)) (label 0)
    let res = eval expr eEmpty
    let (_,s) = runState res (identity [(0,Qubit)])
    print s

test2 :: IO () --test two-qubit gate application
test2 = do
    let expr = apply @Deep (circuit ((label 0) ⊗ (label 1)) (hadamard 2) ((label 2) ⊗ (label 3))) ((label 0) ⊗ (label 1))
    let res = eval expr eEmpty
    let (_,s) = runState res (identity [(0,Qubit),(1,Qubit)])
    print s
