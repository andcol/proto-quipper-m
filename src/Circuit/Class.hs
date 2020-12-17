module Circuit.Class where

import Circuit.Types
import Data.Type.Equality
import Data.Proxy

import GHC.TypeLits

class FreshIn (l :: Label) (lc :: LabelContext) --l does not occur as a label in lc (= not OccursIn)
instance FreshIn l '[]
instance ((l1 == l2) ~ False, FreshIn l1 rc) => FreshIn l1 ('(l2, w) : rc)

class Disjoint (lc1 :: LabelContext) (lc2 :: LabelContext) --The labels in lc1 and the labels in lc2 have empty intersection
instance Disjoint '[] lc2
instance (FreshIn l lc2, Disjoint rc1 lc2) => Disjoint ('(l, w) : rc1) lc2

class SemanticOf (m :: [WireType]) (lc :: LabelContext) | lc -> m --[[lc]] = [[m]] = [[m1]] * [[m2]] * ... * [[mn]]
instance SemanticOf '[w] ('(l, w) : '[])
instance (SemanticOf rt rc, FreshIn l rc) => SemanticOf (w : rt) ('(l, w) : rc)

class OccursIn (l :: Label) (lc :: LabelContext) --l occurs as a label in lc (= not FreshIn)
instance OccursIn l ('(l,w) : '[])
instance (OccursIn l rc) => OccursIn l ('(l',w) : rc)

class AllOccurIn (labels :: [Label]) (lc :: LabelContext) --Each label in labels occurs as a label in lc
instance (OccursIn l lc) => AllOccurIn (l : '[]) lc
instance (OccursIn l lc, AllOccurIn rl lc) => AllOccurIn (l : rl) lc

class Lookup (lc :: LabelContext) (l :: Label) (w :: WireType) | lc l -> w --l has wire type w in lc
instance Lookup ('(l, w) : rc) l w
instance (Lookup rc l w) => Lookup ('(l',w') : rc) l w

class CompatibleOver (lc1 :: LabelContext) (labels :: [Label]) (lc2 :: LabelContext) --labels and lc2 have the same size and for every i, the wire type of the i-th label of lc2 in lc2 and the type of the i-th label of labels in lc1 match
instance (Lookup lc1 l w) => CompatibleOver lc1 (l : '[]) ('(l',w) : '[])
instance (Lookup lc1 l w, CompatibleOver lc1 lr rc2) => CompatibleOver lc1 (l : lr) ('(l',w) : rc2)

class RemoveLabel (lc1 :: LabelContext) (l :: label) (lc2 :: LabelContext) | lc1 l -> lc2 --lc2 = lc1 without l
instance RemoveLabel '[] l '[]
instance RemoveLabel ('(l, w) : rc1) l rc1
instance ((l == l') ~ False, RemoveLabel rc1 l rc2') => RemoveLabel ('(l', w) : rc1) l ('(l', w) : rc2')

class RemoveLabels (lc1 :: LabelContext) (labels :: [Label]) (lc2 :: LabelContext) | lc1 labels -> lc2 --lc2 = lc1 without all the labels in labels
instance (RemoveLabel lc l lc') => RemoveLabels lc (l : '[]) lc'
instance (RemoveLabel lc l lc', RemoveLabels lc' lr lc'') => RemoveLabels lc (l : lr) lc''

class (Disjoint lc1 lc2) => Merge (lc1 :: LabelContext) (lc2 :: LabelContext) (lc :: LabelContext) | lc1 lc2 -> lc, lc1 lc -> lc2 --lc = lc1 + lc2 if they are disjoint
instance Merge '[] lc2 lc2
instance (FreshIn l lc2, Merge rc1 lc2 lc2') => Merge ('(l,w) : rc1) lc2 ('(l,w) : lc2') --check for gods sake

class LabelsOf (lc :: LabelContext) (labels :: [Label]) | lc -> labels
instance LabelsOf '[] '[]
instance (LabelsOf rc lr) => LabelsOf ('(l, w) : rc) (l : lr)

class SameSize (l1 :: [a]) (l2 :: [b])
instance SameSize '[] '[]
instance SameSize r1 r2 => SameSize (e1 : r1) (e2 : r2)

class FromLabelsAndSemantics (labels :: [Label]) (semantics :: [WireType]) (lc :: LabelContext) | labels semantics -> lc
instance FromLabelsAndSemantics '[] '[] '[]
instance FromLabelsAndSemantics lr sr lcr => FromLabelsAndSemantics (l ': lr) (w ': sr) ('(l,w) : lcr)

--PRODUCE FRESH LABELS

class MaxLabel (labels :: [Label]) (l :: Label) | labels -> l
instance MaxLabel '[] 0
instance (MaxLabel rl l', (l <=? l') ~ False) => MaxLabel (l : rl) l
instance (MaxLabel rl l', (l <=? l') ~ True) => MaxLabel (l : rl) l'

class MakeFreshLabel (lc :: LabelContext) (l :: Label) | lc -> l
instance MakeFreshLabel '[] 0
instance (LabelsOf lc labels, MaxLabel labels n, n' ~ (n+1)) => MakeFreshLabel lc n'

class LabelRange (start :: Label) (n :: [a]) (range :: [Label]) | start n -> range
instance LabelRange start '[] '[]
instance (LabelRange start n' rr, MaxLabel rr l', l ~ (l'+1)) => LabelRange start (n ': n') (l : rr)

class MakeFreshLabels (lc :: LabelContext) (n :: [a]) (labels :: [Label]) | lc n -> labels
instance (MakeFreshLabel lc start, LabelRange start n labels) => MakeFreshLabels lc n labels


class LabelledCircuit (circ :: LabelContext -> LabelContext -> *) where
    fromGate :: (SemanticOf sig lc1, SemanticOf sig lc2, Disjoint lc1 lc2)
                    => Gate sig -> circ lc1 lc2
    append :: (AllOccurIn targets q2, --All the targets are some output of C
                CompatibleOver q2 targets q3, --D has the right type to be plugged into the target outputs of C
                RemoveLabels q2 targets q7, --q7 = q2 - targets
                MakeFreshLabels q7 q4 outputs, --outputs are n fresh labels, where n is the length of q4
                SemanticOf s q4, --s is the semantics of q4
                FromLabelsAndSemantics outputs s q8, --q8 has the labels of outputs and the semantics of q4
                --LabelsOf q8 outputs, --q8 has the labels of outputs...
                --SemanticOf out q8, -- ...with the same semantics as q4 (i.e. wire types)
                Merge q8 q7 q6) --q6 = q7 + q8
                => circ q1 q2 -> Proxy targets -> circ q3 q4 -> (circ q1 q6, Proxy outputs) --TODO update signature

--REIFICATION OF LABELS AND CONTEXTS

class LabelList (l :: [Label]) where
    reifyLabels :: Proxy l -> [Int]
instance LabelList '[] where
    reifyLabels _ = []
instance (KnownNat x, LabelList r) => LabelList (x ': r) where
    reifyLabels (Proxy :: Proxy (x ': r)) =  (fromIntegral $ natVal (Proxy :: Proxy x)) : (reifyLabels (Proxy :: Proxy r))
