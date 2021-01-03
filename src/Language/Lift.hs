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

--TESTS

--test1 :: IO () --simple "force (lift n)" scenario
--test1 = do
--    let expr = force @Deep $ lift $ label 0 --should not work, but for now labels are typed without Q
--    let res = eval expr eEmpty
--    let (v,_) = runState res (identity [(0,Qubit)]) --initial state is irrelevant
--    print v
--
--test2 :: IO () --same scenario but with inner evaluation
--test2 = do
--    let expr = force @Deep $ lift $ apply (circuit (label 0) (fromGate H) (label 1)) (label 0) --again, should not work, but for now labels are typed without Q
--    let res = eval expr eEmpty
--    let vs = runState res (identity [(0,Qubit)])
--    print vs
