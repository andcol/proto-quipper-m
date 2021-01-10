module Language.Box where

import Prelude hiding ((^))

import Types
import Classes
import Interface hiding (Bang, force, lift)
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive hiding (freshLabels)
import DeepEmbedding

import Language.Core
import Language.Lift

import Control.Monad.State.Lazy hiding (lift)

import GHC.TypeLits
import Data.Proxy
import Unsafe.Coerce
import Data.Type.Bool
import Data.Type.Equality hiding (apply)

--PRELIMINARIES (needed for reification of type contexts)
class KnownCtx (γ :: Ctx) where
    reify :: forall proxy. proxy γ -> LabelContext
    trivialECtx :: forall proxy. proxy γ -> ECtx Deep γ
instance KnownCtx '[] where
    reify _ = []
    trivialECtx _ = eEmpty
instance (KnownNat x, KnownCtx γ') => KnownCtx ('(x,Qubit) : γ') where
    reify _ = (reifyLabel (Proxy :: Proxy x),Qubit) : (reify (Proxy :: Proxy γ'))
    trivialECtx _ = let x = addECtx (Proxy :: Proxy x) (VLabel $ reifyLabel (Proxy :: Proxy x)) (trivialECtx (Proxy :: Proxy γ')) in
        unsafeCoerce x :: ECtx Deep ('(x,Qubit) : γ') --TODO this has to be THOROUGHLY verified

class (KnownCtx γ, MType Deep t) => LabelCtx (γ :: Ctx) (t :: LType) | γ -> t where
    labelVector :: forall proxy. proxy γ -> Deep γ t
instance KnownNat x => LabelCtx '[ '(x, Qubit) ] Qubit where
    labelVector _ = label (Proxy :: Proxy x)
    

type family FreshLabel (γ :: Ctx) :: Nat where
    FreshLabel '[] = 0
    FreshLabel '[ '(x,t) ] = x+1
    FreshLabel ('(x,t) : γ') = FreshLabel γ' --Ctx's are ordered

type family FreshLabels (γ :: Ctx) (n :: Nat) :: [Nat] where
    FreshLabels γ 1 = '[FreshLabel γ]
    FreshLabels γ n = LabelRange (FreshLabel γ) n

type family FreshLabelContext (γ :: Ctx) (t :: LType) :: Ctx where
    FreshLabelContext γ t = Zip (FreshLabels γ (MLen t)) (ToList t)

type family MTypeOf (γ :: Ctx) :: LType where
    MTypeOf '[] = One --sadly necessary for the implementation of labelVector
    MTypeOf '[ '(x,Qubit) ] = Qubit
    MTypeOf ('(x,Qubit) : r) = Qubit ⊗ (MTypeOf r)

type family LabelRange (start :: Nat) (n :: Nat) :: [Nat] where
    LabelRange start 1 = '[start]
    LabelRange start n = start ': (LabelRange (start + 1) (n - 1))

type family (++) (list1 :: [a]) (list2 :: [a]) :: [a] where
    '[] ++ list2 = list2
    list1 ++ '[] = list1
    (e ': rl) ++ list2 = e ': (rl ++ list2)

type family Zip (l1 :: [a]) (l2 :: [b]) :: [(a,b)] where
    Zip '[] l2 = '[]
    Zip l1 '[] = '[]
    Zip (e1 ': r1) (e2 ': r2) = '(e1,e2) ': (Zip r1 r2)

type family MLen (t :: LType) :: Nat where
    MLen Qubit = 1
    MLen (Qubit ⊗ t') = (MLen t') + 1

type family ToList (t :: LType) :: [LType] where
    ToList Qubit = '[Qubit]
    ToList (Qubit ⊗ t') = Qubit ': (ToList t')


--DECLARATION

class (HasCore exp, HasLift exp, HasLolli exp) => HasBox (exp :: Sig) where
    box :: forall (t :: LType) (u :: LType) (γ :: Ctx).
            (MType Deep t, MType Deep u, KnownCtx γ,
            LabelCtx (Zip (FreshLabels γ (MLen t)) (ToList t)) t,
            MergeF (Zip (FreshLabels γ (MLen t)) (ToList t)) '[] ~ Zip (FreshLabels γ (MLen t)) (ToList t),
            Types.Div (Zip (FreshLabels γ (MLen t)) (ToList t)) (Zip (FreshLabels γ (MLen t)) (ToList t)) ~ '[])
                => exp γ (Bang (t ⊸ u)) -> exp γ (Circ t u)

--DEEP EMBEDDING

data BoxExp :: Sig where
    Box :: forall (t :: LType) (u :: LType) (γ :: Ctx).
            (MType Deep t, MType Deep u, KnownCtx γ,
            LabelCtx (Zip (FreshLabels γ (MLen t)) (ToList t)) t,
            MergeF (Zip (FreshLabels γ (MLen t)) (ToList t)) '[] ~ Zip (FreshLabels γ (MLen t)) (ToList t),
            Types.Div (Zip (FreshLabels γ (MLen t)) (ToList t)) (Zip (FreshLabels γ (MLen t)) (ToList t)) ~ '[])
                => Deep γ (Bang (t ⊸ u)) -> BoxExp γ (Circ t u)

instance HasBox Deep where
    box = Dom . Box


instance Domain BoxExp where
    evalDomain (Box (m :: Deep γ (Bang (t ⊸ u)))) ρ = do
        VLift n <- eval m ρ
        --begin subroutine
        let q = Proxy :: Proxy (FreshLabelContext γ t)
        let vecell = unsafeCoerce $ labelVector q :: Deep (FreshLabelContext γ t) t
        let subroutine = n ^ vecell --find a workaround to convince GHC that vecell has type t
        let res = eval subroutine (trivialECtx q)
        let (l',d) = runState res (identity $ reify q)
        --end subroutine
        l <- eval vecell (trivialECtx q) --little more than a formality (vecell is basically already a value)
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

--test2 :: IO () --test two-qubit gate application and boxing
--test2 = do
--    let expr = box @Deep $ lift $ λ $ \l -> apply (circuit ((VLabel 0) `VPair` (VLabel 1)) (hadamard 2) ((VLabel 2) `VPair` (VLabel 3))) l
--    let res = eval expr eEmpty
--    let (v,s) = runState res (identity [(0,Qubit),(1,Qubit)])
--    print v





