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

class (HasCore exp, HasLift exp, HasLolli exp) => HasBox (exp :: Sig) where
    box :: (MType exp t, MType exp u) => [WireType] -> exp γ (Bang (t ⊸ u)) -> exp γ (Circ t u)

--DEEP EMBEDDING

data BoxExp :: Sig where
    Box :: (MType Deep t, MType Deep u) => [WireType] -> Deep γ (Bang (t ⊸ u)) -> BoxExp γ (Circ t u)

instance HasBox Deep where
    box t m = Dom $ Box t m

instance Domain BoxExp where
    evalDomain (Box t (m :: Deep γ (Bang (t ⊸ u)))) ρ = do
        VLift n <- eval m ρ
        let q = zip (freshLabels n (length t)) t
        let lTerm = fromLabelContext q
        l <- eval lTerm eEmpty
        let subroutine = n ^ lTerm
        let res = eval subroutine eEmpty
        let (l',d) = runState res (identity q)
        return $ VCirc l d l'
        --this function is such a mess


--FRESHLABELS
--I don't like this solution, it feels wrong
--At the same time I haven't found a better solution for this problem in Haskell
--But I refuse to believe there isn't one

freshLabel :: Deep γ τ -> Label
freshLabel (Var _) = 0
freshLabel (Dom e) = freshLabelDom e

freshLabels :: Deep γ τ -> Int -> [Label]
freshLabels e n = let start = freshLabel e in [start + i | i <- [0..n-1]]

class DomainWithLabels (dom :: Sig) where
    freshLabelDom :: dom γ τ -> Label

instance DomainWithLabels CoreExp where
    freshLabelDom (Label id) = id+1
    freshLabelDom (Circuit _ _ _) = 0
    freshLabelDom (Apply m n) = max (freshLabel m) (freshLabel n)

instance DomainWithLabels LiftExp where
    freshLabelDom (Lift m) = freshLabel m
    freshLabelDom (Force m) = freshLabel m

instance DomainWithLabels BoxExp where
    freshLabelDom (Box _ m) = freshLabel m

instance DomainWithLabels TensorExp where
    freshLabelDom (Pair m n) = max (freshLabel m) (freshLabel n)
    freshLabelDom (LetPair _ _ m n) = max (freshLabel m) (freshLabel n)

instance DomainWithLabels LolliExp where
    freshLabelDom (Abs _ m) = freshLabel m
    freshLabelDom (App m n) = max (freshLabel m) (freshLabel n)

instance (Domain dom) => DomainWithLabels dom where --sort of a default
    freshLabelDom _ = 0

--TESTS

instance (Show (LVal Deep l), Show (LVal Deep r)) => Show (LVal Deep (l ⊗ r)) where
    show (VPair x y) = "⟨" ++ (show x) ++ ", " ++ (show y) ++ "⟩"

instance (Show (LVal Deep i), Show (LVal Deep o)) => Show (LVal Deep (Circ i o)) where
    show (VCirc l d l') = "(" ++ (show l) ++ ", " ++ (show d) ++ ", " ++ (show l') ++ ")"

test1 :: IO () --tests single-qubit gate application and boxing
test1 = do
    let expr = box [Qubit] $ lift $ λ $ \l -> apply @Deep (circuit (label 0) (fromGate H) (label 1)) l
    let res = eval expr eEmpty
    let (v,s) = runState res (identity [(0,Qubit)])
    print v

test2 :: IO () --test two-qubit gate application and boxing
test2 = do
    let expr = box [Qubit, Qubit] $ lift $ λ $ \l -> apply @Deep (circuit ((label 0) ⊗ (label 1)) (hadamard 2) ((label 2) ⊗ (label 3))) l
    let res = eval expr eEmpty
    let (v,s) = runState res (identity [(0,Qubit),(1,Qubit)])
    print v





