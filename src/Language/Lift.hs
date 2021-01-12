module Language.Lift where

import Types
import Classes
import Interface hiding (Bang, force, lift)
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive
import DeepEmbedding

import Control.Monad.State.Lazy hiding (lift)

import Language.Core

data BangSig exp = MkBang exp
type Bang a = MkLType ('MkBang a)

class HasLift (exp :: Sig) where
    lift :: exp '[] a -> exp '[] (Bang a)
    force :: exp γ (Bang a) -> exp γ a

--DEEP EMBEDDING

data LiftExp :: Sig where
    Lift :: Deep '[] a -> LiftExp '[] (Bang a)
    Force :: Deep γ (Bang a) -> LiftExp γ a

instance HasLift Deep where
    lift = Dom . Lift
    force = Dom . Force

data instance LVal Deep (Bang a) where
    VLift :: Deep '[] a -> LVal Deep (Bang a) --a lift-value is a closure just like a lambda-abstraction value, but it is always empty

instance Domain LiftExp where
    evalDomain (Lift m) _ = return $ VLift m
    evalDomain (Force m) ρ = do
        VLift n <- eval m ρ
        eval n eEmpty
