module Language.Util where

import Circuit.Dynamic.Class
import Language.Core
import Language.Lift
import Language.Box

import Types
import DeepEmbedding

import Data.Proxy

--FRESHLABELS
--I don't like this solution, it feels wrong
--At the same time I haven't found a better solution for this problem in Haskell
--But I refuse to believe there isn't one

freshLabel :: Deep γ τ -> Label
freshLabel (Var _) = 0
freshLabel (Dom e) = freshLabelDom e

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
    freshLabelDom (Box m) = freshLabel m

instance DomainWithLabels TensorExp where
    freshLabelDom (Pair m n) = max (freshLabel m) (freshLabel n)
    freshLabelDom (LetPair _ _ m n) = max (freshLabel m) (freshLabel n)

instance DomainWithLabels LolliExp where
    freshLabelDom (Abs _ m) = freshLabel m
    freshLabelDom (App m n) = max (freshLabel m) (freshLabel n)

instance (Domain dom) => DomainWithLabels dom where --sort of a default
    freshLabelDom _ = 0