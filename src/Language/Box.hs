module Language.Box where

import Prelude hiding ((^))
import LNLHask
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive hiding (freshLabels)
import Language.Core
import Language.Lift

import Control.Monad.State.Lazy hiding (lift)

import Data.Proxy
import Language.LabelContexts --manipulation and reification of type-level label contexts


-- INTERFACE

class (HasCore exp, HasLift exp, HasLolli exp) => HasBox (exp :: Sig) where
    box :: forall (t :: LType) (u :: LType) (γ :: Ctx).
            (MType Deep t, MType Deep u, KnownCtx γ,
            -- The remaining constraints do not reflect the logic of box
            --  they are trivial equivalences that can only be resolved by GHC once a concrete type is supplied (i.e. not now)
            LabelCtx (FreshLabelContext γ t) t, --True because FreshLabelContexts γ t yields a valid Ctx type with only MTypes by definition
            MergeF (FreshLabelContext γ t) '[] ~ FreshLabelContext γ t, -- Trivially true (but MergeF is defined inductively on the first argument)
            Div (FreshLabelContext γ t) (FreshLabelContext γ t) ~ '[]) -- Trivially true (if I remove all xs from xs I get [])
                => exp γ (Bang (t ⊸ u)) -> exp γ (Circ t u)


--DEEP EMBEDDING

data BoxExp :: Sig where
    Box :: forall (t :: LType) (u :: LType) (γ :: Ctx).
            (MType Deep t, MType Deep u, KnownCtx γ,
            LabelCtx (FreshLabelContext γ t) t, --
            MergeF (FreshLabelContext γ t) '[] ~ FreshLabelContext γ t,
            Div (FreshLabelContext γ t) (FreshLabelContext γ t) ~ '[])
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






