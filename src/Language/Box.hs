module Language.Box where

import Prelude hiding ((^))

import Types
import Classes
import Interface hiding (Bang, force, lift)
import DeepEmbedding

import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive hiding (freshLabels)

import Language.Core
import Language.Lift
import Language.LabelContexts --manipulation and reification of type-level label contexts

import Control.Monad.State.Lazy hiding (lift)

import Data.Proxy


--DECLARATION

class (HasCore exp, HasLift exp, HasLolli exp) => HasBox (exp :: Sig) where
    box :: forall (t :: LType) (u :: LType) (γ :: Ctx).
            (MType Deep t, MType Deep u, KnownCtx γ,
            -- The remaining constraints do not reflect the logic of box
            --  they are trivial equivalences that can only be resolved by GHC once a concrete type is supplied (i.e. not now)
            LabelCtx (FreshLabelContext γ t) t, --True because FreshLabelContexts γ t yields a valid Ctx type with only MTypes by definition
            MergeF (FreshLabelContext γ t) '[] ~ FreshLabelContext γ t, -- Trivially true (but MergeF is defined inductively on the first argument)
            Types.Div (FreshLabelContext γ t) (FreshLabelContext γ t) ~ '[]) -- Trivially true (if I remove all xs from xs I get [])
                => exp γ (Bang (t ⊸ u)) -> exp γ (Circ t u)

--DEEP EMBEDDING

data BoxExp :: Sig where
    Box :: forall (t :: LType) (u :: LType) (γ :: Ctx).
            (MType Deep t, MType Deep u, KnownCtx γ,
            LabelCtx (FreshLabelContext γ t) t, --
            MergeF (FreshLabelContext γ t) '[] ~ FreshLabelContext γ t,
            Types.Div (FreshLabelContext γ t) (FreshLabelContext γ t) ~ '[])
                => Deep γ (Bang (t ⊸ u)) -> BoxExp γ (Circ t u)

instance HasBox Deep where
    box = Dom . Box


instance Domain BoxExp where
    evalDomain (Box (m :: Deep γ (Bang (t ⊸ u)))) ρ = do
        VLift n <- eval m ρ
        --begin subroutine
        let q = Proxy :: Proxy (FreshLabelContext γ t)
        let vecell = labelVector q
        let subroutine = n ^ vecell
        let res = eval subroutine (trivialECtx q)
        let idQ = (identity $ reify q)
        let (l',d) = runState res idQ
        --end subroutine
        l <- eval vecell (trivialECtx q) --little more than a formality (in practice vecell is already a value)
        return $ VCirc l d l'


--TESTS

instance (Show (LVal Deep l), Show (LVal Deep r)) => Show (LVal Deep (l ⊗ r)) where
    show (VPair x y) = "⟨" ++ (show x) ++ ", " ++ (show y) ++ "⟩"

instance (Show (LVal Deep i), Show (LVal Deep o)) => Show (LVal Deep (Circ i o)) where
    show (VCirc l d l') = "(" ++ (show l) ++ ", " ++ (show d) ++ ", " ++ (show l') ++ ")"

test1 :: IO () --tests single-qubit gate application and boxing
test1 = do
    let expr = box @Deep $ lift $ λ $ \l -> apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) l
    let res = eval expr eEmpty
    let out = runState res (identity [(0,Qubit)])
    print out

test2 :: IO () --test two-qubit gate application and boxing
test2 = do
    let expr = box @Deep $ lift $ λ $ \l -> apply (circuit ((VLabel 0) `VPair` (VLabel 1)) (hadamard 2) ((VLabel 2) `VPair` (VLabel 3))) l
    let res = eval expr eEmpty
    let (v,s) = runState res (identity [(0,Qubit),(1,Qubit)])
    print v





