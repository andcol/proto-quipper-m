module Circuit.Class where

import Circuit.Gate
import Circuit.LabelContext
import Data.Type.Equality
import Data.Proxy

import GHC.TypeLits


class LabelledCircuit (circ :: LabelContext -> LabelContext -> *) where
    fromGate :: (n ~ SizeOf sig,
                    lc1 ~ Zip (LabelRange 0 n) sig,
                    lc2 ~ Zip (LabelRange n n) sig)
                    => Gate sig -> circ lc1 lc2
    append :: (AllOccurIn targets q2, --Every target is some output of C
                CompatibleOver q2 targets q3, --D has the right type to be plugged into the target outputs of C
                q7 ~ RemoveLabels targets q2, --q7 = q2 - targets
                outputs ~ MakeFreshLabels q7 (SizeOf q4), --outputs are n labels, fresh in the unused output context, where n is the length of q4
                q8 ~ (Zip outputs (SemanticsOf q4)), --q8 has the labels of outputs and the semantics of q4
                q6 ~ Unwrap (Merge q7 q8)) --q6 = q7 + q8 if q7 and q8 are disjoint (fails otherwise)
                => circ q1 q2 -> Proxy targets -> circ q3 q4 -> (circ q1 q6, Proxy outputs) --TODO update signature
