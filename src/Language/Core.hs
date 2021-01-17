module Language.Core where

import Prelude hiding ((^))
import LNLHask
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Control.Monad.State.Lazy

import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce


-- LINEAR TYPES

type UnderlyingCircuit = Circuit --choose the implementation of the circuit here!

class MType (exp :: Sig) (t :: LType) where
    labels :: LVal exp t -> [Label] --extracts the labels of a value of MType T
    toWireType :: Proxy exp -> Proxy t -> [WireType]
    fromLabelContext :: LabelContext -> LVal exp t --constructs a value of MType T from a label context

sig :: MType exp t => LVal exp t -> [WireType] --extracts a runtime representation (WireType) of the MType T of a value
sig (_ :: LVal exp t) = toWireType (Proxy :: (Proxy exp)) (Proxy :: Proxy t)

toLabelContext :: MType exp t => LVal exp t -> LabelContext --retrieves the label context Q such that ∅;Q ⊢ ℓ : t
toLabelContext vl = zip (labels vl) (sig vl)

size :: MType exp t => Proxy exp -> Proxy t -> Int
size expp tp = length $ toWireType expp tp

data QubitSig exp = MkQubit
type Qubit = MkLType 'MkQubit

instance MType Deep Qubit where
    labels (VLabel id) = [id]
    toWireType _ _ = [Qubit]
    fromLabelContext [(l,Qubit)] = VLabel l
instance (MType Deep t) => MType Deep (Qubit ⊗ t) where
    labels (VPair (VLabel id) x) = id : (labels x)
    toWireType _ (Proxy :: Proxy (Qubit ⊗ t)) = Qubit : (toWireType (Proxy :: Proxy Deep) (Proxy :: Proxy t))
    fromLabelContext ((l,Qubit):lcr) = VPair (VLabel l) (fromLabelContext lcr)

data CircSig exp = MkCirc exp exp
type Circ t u = MkLType ('MkCirc t u)


-- INTERFACE

class HasTensor exp => HasCore exp where
    label :: StaticLabel label => Proxy label -> exp '[ '(label, Qubit) ] Qubit
    circuit :: (MType exp t, MType exp u) => LVal exp t -> UnderlyingCircuit -> LVal exp u -> exp '[] (Circ t u)
    apply :: (MType exp t, MType exp u, CMerge γ1 γ2 γ) => exp γ1 (Circ t u) -> exp γ2 t -> exp γ u


--DEEP EMBEDDING

type instance Effect Deep = State UnderlyingCircuit 
    
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

instance Domain CoreExp where
    evalDomain (Label staticLabel) ρ = return $ VLabel (reifyLabel staticLabel)
    evalDomain (Circuit l c l') eEmpty =
        if (toLabelContext l) == (inputsOf c) && (toLabelContext l') == (outputsOf c)
        --akin to the compile-time check "c ∈ Mℒ(Q,Q') where Q and Q' type vl and vl' in t and u respectively"
            then return $ VCirc l c l'
            else error ("mismatch between term-level labels and actual circuit labels: "
                        ++ (show $ toLabelContext l) ++ " vs. " ++ (show $ inputsOf c) ++ " and "
                        ++ (show $ toLabelContext l') ++ " vs. " ++ (show $ outputsOf c) ++ ".")
    evalDomain (Apply (circuit :: Deep γ1 (Circ t u)) (targets :: Deep γ2 t)) ρ = do
        VCirc l d l' <- eval circuit ρ1
        targetVec <- eval targets ρ2
        outputVec <- appendM (labels targetVec) d
        return $ fromLabelContext (zip outputVec (sig l'))
        where
            (ρ1,ρ2) = splitECtx @γ1 @γ2 ρ

            