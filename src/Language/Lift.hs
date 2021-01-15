module Language.Lift where

import LNLHask
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive
import Language.Core

import Control.Monad.State.Lazy hiding (lift)


-- LINEAR TYPES

data BangSig exp = MkBang exp
type Bang a = MkLType ('MkBang a)


-- INTERFACE

class HasLift (exp :: Sig) where
    lift :: exp '[] a -> exp '[] (Bang a)
    force :: exp γ (Bang a) -> exp γ a


-- DEEP EMBEDDING

data LiftExp :: Sig where
    Lift :: Deep '[] a -> LiftExp '[] (Bang a)
    Force :: Deep γ (Bang a) -> LiftExp γ a

instance HasLift Deep where
    lift = Dom . Lift
    force = Dom . Force

data instance LVal Deep (Bang a) where
    VLift :: Deep '[] a -> LVal Deep (Bang a)

instance Domain LiftExp where
    evalDomain (Lift m) _ = return $ VLift m
    evalDomain (Force m) ρ = do
        VLift n <- eval m ρ
        eval n eEmpty
