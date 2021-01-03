module Language.Core where

import Types
import Classes
import Interface
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive
import DeepEmbedding

import Control.Monad.State.Lazy

import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

--PRELIMINARIES (needed for reification of type contexts)
class LabelsOnly (γ :: Ctx) where
    reify :: Proxy γ -> LabelContext
    trivialECtx :: Proxy γ -> ECtx Deep γ
instance LabelsOnly '[] where
    reify _ = []
    trivialECtx _ = eEmpty
instance (KnownNat x, LabelsOnly γ') => LabelsOnly ('(x,Qubit) : γ') where
    reify _ = (reifyLabel (Proxy :: Proxy x),Qubit) : (reify (Proxy :: Proxy γ'))
    trivialECtx _ = let x = addECtx (Proxy :: Proxy x) (VLabel $ reifyLabel (Proxy :: Proxy x)) (trivialECtx (Proxy :: Proxy γ')) in
        unsafeCoerce x :: ECtx Deep ('(x,Qubit) : γ') --TODO this has to be THOROUGHLY verified

--DECLARATION

type UnderlyingCircuit = Circuit --choose the implementation of the circuit here!

data QubitSig exp = MkQubit
type Qubit = MkLType 'MkQubit

data CircSig exp = MkCirc exp exp
type Circ t u = MkLType ('MkCirc t u)

class MType (exp :: Sig) (t :: LType) where
    labels :: LVal exp t -> [Label] --extracts the labels of a value of MType T
    signature :: LVal exp t -> [WireType] --extracts a runtime representation (WireType) of the MType T of a value
    fromLabelContext :: LabelContext -> LVal exp t --constructs a value of MType T from a label context
    toLabelContext :: LVal exp t -> LabelContext --retrieves the label context Q such that \emptyset;Q |- \vec\ell : T
    toLabelContext vl = zip (labels vl) (signature vl)

class HasTensor exp => HasCore exp where
    label :: StaticLabel label => Proxy label -> exp '[ '(label, Qubit) ] Qubit
    circuit :: (MType exp t, MType exp u) => LVal exp t -> UnderlyingCircuit -> LVal exp u -> exp '[] (Circ t u)
    apply :: (MType exp t, MType exp u, CMerge γ1 γ2 γ) => exp γ1 (Circ t u) -> exp γ2 t -> exp γ u


--DEEP EMBEDDING

type instance Effect _ = State UnderlyingCircuit 
    

data CoreExp :: Sig where
    Label :: StaticLabel label => Proxy label -> CoreExp '[ '(label, Qubit) ] Qubit
    Circuit :: (MType Deep t, MType Deep u) => LVal Deep t -> UnderlyingCircuit -> LVal Deep u -> CoreExp '[] (Circ t u)
    Apply :: (MType Deep t, MType Deep u, CMerge γ1 γ2 γ) => Deep γ1 (Circ t u) -> Deep γ2 t -> CoreExp γ u

instance HasCore Deep where
    label = Dom . Label
    circuit l c l' = Dom $ Circuit l c l'
    apply c l = Dom $ Apply c l

data instance LVal Deep Qubit = VLabel Label deriving (Show)
data instance LVal Deep (Circ t u)  where
    VCirc :: (MType Deep t, MType Deep u) => LVal Deep t -> UnderlyingCircuit -> LVal Deep u -> LVal Deep (Circ t u)

instance MType Deep Qubit where
    labels (VLabel id) = [id]
    signature _ = [Qubit]
    fromLabelContext [(l,Qubit)] = VLabel l
instance (MType Deep t) => MType Deep (Qubit ⊗ t) where
    labels (VPair (VLabel id) x) = id : (labels x)
    signature (VPair _ r) = Qubit : (signature r)
    fromLabelContext ((l,Qubit):lcr) = VPair (VLabel l) (fromLabelContext lcr)


instance Domain CoreExp where
    evalDomain (Label staticLabel) ρ = return $ VLabel (reifyLabel staticLabel)
    evalDomain (Circuit l c l') eEmpty =
        if (toLabelContext l) == (inputsOf c) && (toLabelContext l') == (outputsOf c) --akin to the compile-time check "c ∈ Mℒ(Q,Q') where Q and Q' type vl and vl' in t and u respectively"
            then return $ VCirc l c l'
            else error "mismatch between term-level labels and actual circuit labels"
    evalDomain (Apply (circuit :: Deep γ1 (Circ t u)) (targets :: Deep γ2 t)) ρ = do
        VCirc l d l' <- eval circuit ρ1
        targetVec <- eval targets ρ2
        outputVec <- appendM (labels targetVec) d
        return $ fromLabelContext (zip outputVec (signature l'))
        where
            (ρ1,ρ2) = splitECtx @γ1 @γ2 ρ

--TESTS

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
